/// Lua ByteCode object wrapper
mod bytecode;
use bytecode::*;

mod dump;
use dump::lua_dump_module;

mod register;
use register::*;

mod utils;
use utils::*;

mod label;
mod vm;
use label::{InstLabel, LabelPtr};

#[cfg(test)]
mod test;

use crate::backend::*;
use crate::parser::{BitValue, LiteralValue, Operator};
use crate::prelude::*;

use indexmap::{IndexMap, IndexSet};
use log::*;
use smallvec::{smallvec, SmallVec};
use std::mem;
use std::rc::Rc;

type ConstIdx = u8;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct LuaType: u8 {
        const NONE          = 0xff;

        const NIL           = 0;
        const BOOLEAN       = 1;
        const LIGHTUSERDATA = 2;
        const NUMBER        = 3;
        const STRING        = 4;
        const TABLE         = 5;
        const FUNCTION      = 6;
        const USERDATA      = 7;
        const THREAD        = 8;
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub(crate) struct LuaVarKind: u8 {
        // regular
        const VDKREG        = 0;
        // constant
        const RDKCONST      = 1;
        // to-be-closed
        const RDKTOCLOSE    = 2;
        // compile-time constant
        const RDKCTC        = 3;
    }
}

#[derive(Debug, Clone, Copy)]
enum LuaAccessMode {
    None,
    Call(usize),
    ReadSymbol,
    WriteRegister,
    // Read value into register or literal
    LoadNewRegister,
    // Read value into register only
    ReadRegisterOnly,
    // Load value into given register
    LoadExistRegister,
    Write,
}

#[derive(Clone)]
struct LuaBackendStates {
    variable: Option<Rc<Variable>>,
    registers: SmallVec8<Reg>,
    const_idx: Option<ConstIdx>,
    scope: Option<Scope>,
    error: bool,
    access_mode: LuaAccessMode,
    expr_exit_label: Option<LabelPtr>,
}

impl LuaBackendStates {
    fn rk(&self) -> RK {
        if !self.registers.is_empty() {
            RK::R(self.registers[0])
        } else if let Some(k) = self.const_idx {
            RK::K(k)
        } else {
            unreachable!()
        }
    }
}

impl Default for LuaBackendStates {
    fn default() -> Self {
        Self {
            variable: None,
            registers: smallvec![],
            scope: None,
            error: false,
            access_mode: LuaAccessMode::None,
            const_idx: None,
            expr_exit_label: None,
        }
    }
}

pub struct LuaBackend {
    mgr: UnitsManager,
    app: ModuleContext,
    byte_codes: Vec<LuaByteCode>,
    states: SmallVec<[LuaBackendStates; 32]>,
    local_function: Option<Function>,
    local_proto: Option<Prototype>,
    reg_mgr: RegisterManager,

    // tmp values for generating function
    upvalue_table: IndexMap<StString, LuaUpValue>,
    constants: IndexSet<LuaConstants>,
    labels: SmallVec<[LabelPtr; 32]>,
}

impl LuaBackend {
    #[inline]
    fn code_gettabup(&mut self, dst: Reg, k: ConstIdx) {
        self.push_code(LuaByteCode::GetTabUp(dst, 0, k));
    }

    #[inline]
    fn code_settabup(&mut self, up_idx: u8, rk: RK) {
        self.push_code(LuaByteCode::SetTabUp(Reg::R(0), up_idx, rk));
    }

    #[inline]
    fn code_move(&mut self, from: Reg, to: Reg) {
        self.push_code(LuaByteCode::Move(to, from))
    }

    #[inline]
    fn code_load(&mut self, dst: Reg, rk: RK) {
        match rk {
            RK::R(r) => self.code_move(r, dst),
            RK::K(v) => self.code_load_literal(dst, &LiteralValue::Byte(v)),
        }
    }

    fn code_load_literal(&mut self, r: Reg, v: &LiteralValue) {
        // if literal can use LoadI instructions
        if let Some(v) = try_fit_sbx(v) {
            self.push_code(LuaByteCode::LoadI(r, v));
            return;
        }

        let constant_index = self.add_constant(v);
        self.code_load_constant(r, constant_index)
    }

    #[inline]
    fn code_load_constant(&mut self, r: Reg, k: ConstIdx) {
        self.push_code(LuaByteCode::LoadK(r, k))
    }

    #[inline]
    fn code_jmp_fixed(&mut self, offset: i32) -> usize {
        self.push_code(LuaByteCode::Jmp(offset));
        self.byte_codes.len() - 1
    }

    #[inline]
    fn code_jmp(&mut self, label: LabelPtr) {
        let fixup = self.code_jmp_fixed(0);
        label.borrow_mut().fixup_instructions.push(fixup);
    }

    #[inline]
    fn code_fixup_jmp(&mut self, index: usize, offset: i32) {
        let new_code = LuaByteCode::Jmp(offset);
        let _ = mem::replace(&mut self.byte_codes[index], new_code);
    }

    #[inline]
    fn push_code(&mut self, code: LuaByteCode) {
        trace!("Code-Lua: {:?}", code);
        self.byte_codes.push(code)
    }

    #[inline]
    fn code_add(&mut self, dst: Reg, op0: RK, op1: RK) {
        let addk = self.add_string_constant("__add");

        match (op0, op1) {
            (RK::K(k), RK::R(r)) | (RK::R(r), RK::K(k)) => {
                self.push_code(LuaByteCode::AddK(dst, r, k));
                self.push_code(LuaByteCode::MMBinK(dst, k, addk));
            }
            (RK::K(k1), RK::K(k2)) => {
                self.push_code(LuaByteCode::LoadK(dst, k1));
                self.push_code(LuaByteCode::AddK(dst, dst, k2));
                self.push_code(LuaByteCode::MMBinK(dst, k2, addk));
            }
            (RK::R(r1), RK::R(r2)) => {
                self.push_code(LuaByteCode::Add(dst, r1, r2));
                self.push_code(LuaByteCode::MMBin(r1, r2, addk));
            }
        }
    }

    #[inline]
    fn code_eq(&mut self, dst: Reg, op0: RK, op1: RK) {
        self.code_load(dst, op0);

        match op1 {
            RK::R(r) => self.push_code(LuaByteCode::Eq(dst, r, 0)),
            RK::K(k) => {
                // Test constants can load into i8
                if let Some(ki8) = self.constants[k as usize].as_lua_i8() {
                    self.push_code(LuaByteCode::EQI(dst, ki8, false))
                } else {
                    // use extra register to compare
                    let r = self.reg_mgr.alloc_hard();

                    self.code_load_constant(r, k);
                    self.push_code(LuaByteCode::Eq(dst, r, 0));
                    self.reg_mgr.free(&r);
                }
            }
        }
    }

    #[inline]
    fn add_constant(&mut self, v: &LiteralValue) -> ConstIdx {
        match v {
            LiteralValue::String(s) => self.add_string_constant(s),
            LiteralValue::Bit(BitValue::Zero) => self.add_integer_constant(0),
            LiteralValue::Bit(BitValue::One) => self.add_integer_constant(1),
            LiteralValue::DInt(i) => self.add_integer_constant(*i as i64),
            LiteralValue::UInt(i) => self.add_integer_constant(*i as i64),
            LiteralValue::Real(s) | LiteralValue::LReal(s) => {
                let f: f64 = s.parse().unwrap();
                self.add_float_constant(f)
            }
            _ => panic!("Add literal {:?} failed", v),
        }
    }

    #[inline]
    fn current_application(&self) -> ModuleContext {
        self.app.clone()
    }

    fn push_attribute_with_scope(&mut self, scope: Scope) {
        let attr = LuaBackendStates {
            scope: Some(scope),
            ..Default::default()
        };

        self.states.push(attr)
    }

    fn push_access_attribute(&mut self, access: LuaAccessMode) {
        let attr = LuaBackendStates {
            scope: self.top_attribute().scope.clone(),
            access_mode: access,
            ..Default::default()
        };

        self.states.push(attr)
    }

    fn push_default_attribute(&mut self) {
        let attr = LuaBackendStates {
            scope: self.top_attribute().scope.clone(),
            ..Default::default()
        };

        self.states.push(attr);
    }

    fn push_exit_label(&mut self, exit_label: LabelPtr) {
        let attr = LuaBackendStates {
            scope: self.top_attribute().scope.clone(),
            expr_exit_label: Some(exit_label),
            ..Default::default()
        };

        self.states.push(attr);
    }

    #[inline]
    fn pop_attribute(&mut self) -> LuaBackendStates {
        self.states.pop().unwrap()
    }

    #[inline]
    fn top_attribute(&mut self) -> &mut LuaBackendStates {
        self.states.last_mut().unwrap()
    }

    /// Return current scope, will be panic if scope not set
    #[inline]
    fn current_scope(&mut self) -> Scope {
        self.top_attribute().clone().scope.unwrap()
    }

    #[inline]
    fn add_string_constant<S: AsRef<str>>(&mut self, s: S) -> ConstIdx {
        let constant = LuaConstants::String(s.as_ref().to_owned());
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstIdx
    }

    #[inline]
    fn add_integer_constant(&mut self, i: i64) -> ConstIdx {
        let constant = LuaConstants::Integer(i);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstIdx
    }

    #[inline]
    fn add_float_constant(&mut self, f: f64) -> ConstIdx {
        let constant = LuaConstants::Float(f);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstIdx
    }
}

impl CodeGenBackend for LuaBackend {
    type Label = LabelPtr;

    fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {
            mgr,
            app,
            byte_codes: Vec::with_capacity(1024 * 2), // 2KiB
            states: smallvec![],
            local_function: None,
            local_proto: None,
            constants: IndexSet::new(),
            upvalue_table: IndexMap::new(),
            reg_mgr: RegisterManager::new(),
            labels: smallvec![],
        }
    }

    fn gen_function(&mut self, func: usize) -> Result<Box<dyn CompiledCode>, CodeGenError> {
        let app = self.app.read();
        let f = app
            .get_function(func)
            .ok_or(CodeGenError::FunctionNotDefined(func))?
            .clone();
        let p = app.get_declaration_by_id(func).cloned();

        self.local_function = Some(f.clone());
        self.local_proto = p;
        drop(app);

        let app_id = self.app.read().id();
        let fun_scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(func));

        // generate VarArgPrep
        if let Some(p) = &self.local_proto {
            if is_vararg(p) {
                self.push_code(LuaByteCode::VarArgPrep(0));
            }
        }

        // create upvalue table
        // self.upvalues.push(LuaUpValue {
        //     name: None,
        //     stack: 1,
        //     index: 0,
        //     kind: 0,
        // });
        self.upvalue_table.insert(
            StString::empty(),
            LuaUpValue {
                stack: 1,
                index: 0,
                kind: LuaVarKind::VDKREG,
            },
        );

        let mut fun = f.write();
        self.push_attribute_with_scope(fun_scope);
        self.visit_statement_mut(fun.parse_tree_mut());
        self.pop_attribute();

        // generate return
        self.push_code(LuaByteCode::Return(0, 1, 1));

        // jmp relocate
        let labels = mem::take(&mut self.labels);
        for label in labels {
            let label_offset = label.borrow().inst_index.unwrap();
            for inst_offset in &label.borrow().fixup_instructions {
                let fix_offset = label_offset as i32 - (*inst_offset as i32) - 1;
                self.code_fixup_jmp(*inst_offset, fix_offset);
            }
        }

        let byte_codes = mem::take(&mut self.byte_codes);
        let constants = mem::replace(&mut self.constants, IndexSet::new());
        let upvalues = mem::replace(&mut self.upvalue_table, IndexMap::new());

        // reset RegMan and check register is balance
        assert!(self.reg_mgr.check_and_reset());

        Ok(Box::new(LuaCompiledCode {
            byte_codes,
            constants,
            upvalues,
        }))
    }

    /// create a new label
    fn create_label<S: AsRef<str>>(&mut self, label: S) -> Self::Label {
        let label_name: StString = label.as_ref().into();
        let lbl = InstLabel::new(label_name.clone());

        self.labels.push(lbl.clone());
        lbl
    }

    /// insert label, record current insert position
    fn insert_label(&mut self, label: Self::Label) {
        // ensure label is not inserted
        debug_assert!(label.borrow().inst_index.is_none());

        // record insert location
        let inst_index = self.byte_codes.len();
        label.borrow_mut().inst_index = Some(inst_index)
    }

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        let operands = operator.operands_mut();

        self.visit_expression_mut(&mut operands[0]);
    }

    fn get_module_bytes(&mut self, w: &mut dyn Write) -> io::Result<()> {
        lua_dump_module(self, w)
    }
}

impl AstVisitorMut for LuaBackend {
    fn visit_literal_mut(&mut self, literal: &mut LiteralExpression) {
        trace!("LuaGen: literal expression: {:?}", literal);

        let access = self.top_attribute().access_mode;

        // Literals can't WRITE
        assert!(!matches!(access, LuaAccessMode::Write));

        self.top_attribute().const_idx = Some(self.add_constant(literal.literal()))
        // if matches!(access, LuaAccessMode::ReadRegisterOnly) {
        //     let r = match self.top_attribute().registers.first() {
        //         Some(r) => *r,
        //         _ => self.reg_mgr.alloc_hard(),
        //     };

        //     self.code_load(r, literal.literal());
        // } else {
        // }
    }

    fn visit_variable_expression_mut(
        &mut self,
        expr: &mut ExprInfo,
        var_expr: &mut VariableExpression,
    ) {
        let scope = self.current_scope();
        let var = scope.find_variable(var_expr.name());

        // trace!(
        //     "LuaGen: variable expression: {}: {:?}",
        //     var_expr,
        //     var.as_ref().and_then(|x| x.ty())
        // );

        let access_mode = self.top_attribute().access_mode;
        match access_mode {
            // Callee process
            LuaAccessMode::Call(arg_cnt) => {
                assert_eq!(self.top_attribute().registers.len(), 0);

                let arg_regs = self.reg_mgr.alloc_hard_batch(arg_cnt);
                self.top_attribute().registers = arg_regs.into();
                self.top_attribute().const_idx = Some(self.add_string_constant(var_expr.name()));
            }
            // Read Symbol
            LuaAccessMode::ReadSymbol => {
                self.top_attribute().const_idx = Some(self.add_string_constant(var_expr.name()));
            }
            LuaAccessMode::WriteRegister => {
                let dst = self.top_attribute().registers[0];
                let constant_index = self.add_string_constant(var_expr.name());
                self.code_gettabup(dst, constant_index);
            }
            // Write register into stack
            LuaAccessMode::Write => {}
            // Load into register
            LuaAccessMode::LoadNewRegister => {
                let scope = self.top_attribute().scope.as_ref().unwrap();
                if let Some(variable) = scope.find_variable(var_expr.name()) {
                    let const_idx = self.add_string_constant(var_expr.name());
                    let reg = self.reg_mgr.alloc_local_variable(variable.name());

                    self.code_gettabup(reg, const_idx);
                    self.top_attribute().registers = smallvec![reg];
                } else {
                    // TODO: variable not found error
                }
            }
            LuaAccessMode::LoadExistRegister => {
                let r = self.top_attribute().registers[0];

                // TODO: Load variable value into register
                self.code_load_literal(r, &LiteralValue::SInt(0))
            }
            _ => unreachable!("{:?}", access_mode),
        }
    }

    fn visit_call_expression_mut(&mut self, call: &mut CallExpression) {
        trace!("LuaGen: call expression: {}", call);

        let args_count = call.arguments().len();
        self.push_access_attribute(LuaAccessMode::Call(args_count));
        self.visit_expression_mut(call.callee_mut());
        let callee_attr = self.pop_attribute();
        let arg_regs = callee_attr.registers;
        let callee_reg = arg_regs[0];

        // Load Callee from constant table into callee_reg
        self.push_code(LuaByteCode::GetTabUp(
            callee_reg,
            0,
            callee_attr.const_idx.unwrap(),
        ));

        // visit all arguments
        for (idx, arg) in call.arguments_mut().iter_mut().enumerate() {
            self.push_access_attribute(LuaAccessMode::WriteRegister);
            self.top_attribute().registers = smallvec![arg_regs[idx + 1]];
            self.visit_expression_mut(arg);
            self.pop_attribute();
        }

        // free registers
        for r in arg_regs {
            self.reg_mgr.free(&r);
        }

        self.push_code(LuaByteCode::Call(
            callee_reg,
            call.arguments().len() as u8 + 1,
            1,
        ));

        self.reg_mgr.free(&callee_reg);
    }

    fn visit_if_statement_mut(&mut self, _: &mut StmtInfo, ifst: &mut IfStatement) {
        trace!("LuaGen: if statement: {}", ifst.condition());

        let if_exit_label = self.create_label("if-exit");

        self.push_exit_label(if_exit_label.clone());
        self.visit_expression_mut(ifst.condition_mut());
        let attr = self.pop_attribute();

        if let Some(then_ctrl) = ifst.then_controlled_mut() {
            self.visit_statement_mut(then_ctrl);
        }

        self.insert_label(if_exit_label);
        self.reg_mgr.free(&attr.registers[0]);
    }

    fn visit_operator_expression_mut(&mut self, operator: &mut OperatorExpression) {
        trace!("LuaGen: operator expression: {}", operator);

        let op = *operator.op();
        let operands = operator.operands_mut();

        match op {
            // binary operators
            Operator::Less
            | Operator::Plus
            | Operator::LessEqual
            | Operator::Equal
            | Operator::NotEqual
            | Operator::Greater
            | Operator::GreaterEqual => {
                let dest_reg = match self.top_attribute().registers.first() {
                    Some(r) => *r,
                    _ => self.reg_mgr.alloc_hard(),
                };
                let cond = self.top_attribute().expr_exit_label.clone();

                self.push_access_attribute(LuaAccessMode::LoadNewRegister);
                self.visit_expression_mut(&mut operands[0]);
                let rk0 = self.pop_attribute().rk();

                self.push_access_attribute(LuaAccessMode::LoadNewRegister);
                self.visit_expression_mut(&mut operands[1]);
                let rk1 = self.pop_attribute().rk();

                // generate operators
                match op {
                    // a + b
                    Operator::Plus => self.code_add(dest_reg, rk0, rk1),
                    // a = b
                    Operator::Equal => self.code_eq(dest_reg, rk0, rk1),
                    _ => unreachable!(),
                }

                // conditional jump
                if let Some(lbl) = cond {
                    self.code_jmp(lbl);
                }

                if let RK::R(r0) = rk0 {
                    self.reg_mgr.free(&r0);
                }
                if let RK::R(r1) = rk1 {
                    self.reg_mgr.free(&r1);
                }
                self.top_attribute().registers = smallvec![dest_reg];
            }

            _ => unreachable!(),
        }
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        trace!("LuaGen: assignment expression: {}", assign);

        self.push_access_attribute(LuaAccessMode::LoadNewRegister);
        assign.right_mut().accept_mut(self);
        let rhs = self.pop_attribute();

        // Get lhs register
        self.push_access_attribute(LuaAccessMode::ReadSymbol);
        assign.left_mut().accept_mut(self);
        let lhs_constant_index = self.pop_attribute().const_idx.unwrap();
        // let lhs_reg = self.pop_attribute().registers[0];

        if let Some(constant_index) = rhs.const_idx {
            assert_eq!(rhs.registers.len(), 0);
            // self.code_load_constant(lhs_reg, constant_index)
            self.code_settabup(lhs_constant_index, RK::K(constant_index));
        } else {
            assert_eq!(1, rhs.registers.len());
            // assert_ne!(lhs_reg, rhs.registers[0]);

            self.reg_mgr.free(&rhs.registers[0]);
            self.code_settabup(lhs_constant_index, RK::R(rhs.registers[0]));
        }
    }
}
