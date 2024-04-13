/// Lua ByteCode object wrapper
mod bytecode;
use bytecode::*;

mod dump;
mod register;
mod utils;
mod vm;

use crate::backend::*;
use crate::parser::{LiteralValue, Operator};
use crate::prelude::*;

use self::dump::lua_dump_module;
use self::register::*;
use self::utils::*;

use indexmap::IndexSet;
use log::*;
use smallvec::{smallvec, SmallVec};
use std::mem;
use std::rc::Rc;

type ConstantIndex = u8;
type UpValueIndex = u8;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct LuaAccessMode: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const READ              = 0b0000_0000_0000_0001;
        const WRITE             = 0b0000_0000_0000_0010;
        const CALL              = 0b0000_0000_0000_0100;

        // Read constant into register first
        const READ_REG          = 0b0000_0001_0000_0000;
    }
}

#[derive(Clone)]
pub struct LuaBackendStates {
    variable: Option<Rc<Variable>>,
    register: Option<Register>,
    constant_index: Option<ConstantIndex>,
    scope: Option<Scope>,
    error: bool,
    access_mode: LuaAccessMode,
}

impl Default for LuaBackendStates {
    fn default() -> Self {
        Self {
            variable: None,
            register: None,
            scope: None,
            error: false,
            access_mode: LuaAccessMode::NONE,
            constant_index: None,
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
    constants: IndexSet<LuaConstants>,
    reg_mgr: RegisterManager,
    module_upvalues: SmallVec<[LuaConstants; 32]>,
}

impl LuaBackend {
    #[inline]
    fn push_code(&mut self, code: LuaByteCode) {
        debug!("LuaBackend: Push Code {:?}", code);
        self.byte_codes.push(code)
    }

    #[inline]
    fn code_move(&mut self, from: Register, to: Register) {
        self.push_code(LuaByteCode::Move(to, from))
    }

    fn code_load(&mut self, r: Register, v: &LiteralValue) {
        // if literal can use LoadI instructions
        if let Some(v) = try_fit_sbx(v) {
            self.push_code(LuaByteCode::LoadI(r, v));
            return;
        }

        let constant_index = self.add_constant(v);
        self.code_load_constant(r, constant_index)
    }

    #[inline]
    fn code_load_constant(&mut self, r: Register, k: ConstantIndex) {
        self.push_code(LuaByteCode::LoadK(r, k))
    }

    #[inline]
    fn add_constant(&mut self, v: &LiteralValue) -> ConstantIndex {
        match v {
            LiteralValue::String(s) => self.add_string_constant(s),
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

    #[inline]
    fn module_upvalues(&self) -> &SmallVec<[LuaConstants; 32]> {
        &self.module_upvalues
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
    fn add_string_constant<S: AsRef<str>>(&mut self, s: S) -> ConstantIndex {
        let constant = LuaConstants::String(s.as_ref().to_owned());
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstantIndex
    }

    #[inline]
    fn add_integer_constant(&mut self, i: i64) -> ConstantIndex {
        let constant = LuaConstants::Integer(i);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstantIndex
    }

    #[inline]
    fn add_float_constant(&mut self, f: f64) -> ConstantIndex {
        let constant = LuaConstants::Float(f);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx as ConstantIndex
    }
}

impl CodeGenBackend for LuaBackend {
    type Label = usize;

    fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {
            mgr,
            app,
            byte_codes: vec![],
            states: smallvec![],
            local_function: None,
            local_proto: None,
            constants: IndexSet::new(),
            reg_mgr: RegisterManager::new(),
            module_upvalues: smallvec![],
        }
    }

    fn get_module_bytes(&mut self, w: &mut dyn Write) -> io::Result<()> {
        lua_dump_module(self, w)
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

        let mut fun = f.write();
        self.push_attribute_with_scope(fun_scope);
        self.visit_statement_mut(fun.parse_tree_mut());
        self.pop_attribute();

        // generate return
        self.push_code(LuaByteCode::Return(0, 1, 1));

        let byte_codes = mem::take(&mut self.byte_codes);
        let constants = mem::replace(&mut self.constants, IndexSet::new());

        Ok(Box::new(LuaCompiledCode {
            byte_codes,
            constants,
            // TODO:: upvalues
            upvalues: smallvec![LuaUpValue {
                name: None,
                stack: 1,
                index: 0,
                kind: 0
            }],
        }))
    }

    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label {
        0
    }

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        let operands = operator.operands_mut();

        self.visit_expression_mut(&mut operands[0]);
    }
}

impl AstVisitorMut for LuaBackend {
    fn visit_literal_mut(&mut self, literal: &mut LiteralExpression) {
        trace!("LuaGen: literal expression: {:?}", literal);

        // Literals can't WRITE
        assert!(!self
            .top_attribute()
            .access_mode
            .contains(LuaAccessMode::WRITE));

        if self
            .top_attribute()
            .access_mode
            .contains(LuaAccessMode::READ_REG)
        {
            let r = self
                .top_attribute()
                .register
                .unwrap_or_else(|| self.reg_mgr.alloc_hard());
            self.code_load(r, literal.literal());
        } else {
            self.top_attribute().constant_index = Some(self.add_constant(literal.literal()))
        }
    }

    fn visit_variable_expression_mut(&mut self, var_expr: &mut VariableExpression) {
        let scope = self.current_scope();
        let var = scope.find_variable(var_expr.name());

        trace!(
            "LuaGen: variable expression: {}: {:?}",
            var_expr,
            var.and_then(|x| x.ty())
        );

        let access_mode = self.top_attribute().access_mode;
        match access_mode & (LuaAccessMode::READ | LuaAccessMode::WRITE | LuaAccessMode::CALL) {
            // Callee process
            LuaAccessMode::CALL => {
                self.top_attribute().constant_index =
                    Some(self.add_string_constant(var_expr.org_name()))
            }
            // Write register into stack
            LuaAccessMode::WRITE => {}
            // Load into register
            LuaAccessMode::READ => {
                let scope = self.top_attribute().scope.as_ref().unwrap();
                if let Some(variable) = scope.find_variable(var_expr.name()) {
                    let reg = self.reg_mgr.alloc_hard();
                    self.top_attribute().register = Some(reg);

                    // TODO: initialize
                    self.push_code(LuaByteCode::LoadI(reg, 0));
                } else {
                    // TODO: variable not found error
                }
            }
            _ => unreachable!("{:?}", access_mode),
        }
    }

    fn visit_call_expression_mut(&mut self, call: &mut CallExpression) {
        trace!("LuaGen: call expression: {}", call);

        self.push_access_attribute(LuaAccessMode::CALL);
        self.visit_expression_mut(call.callee_mut());
        let callee_index = self.top_attribute().constant_index;
        self.pop_attribute();

        // TODO:
        // self.push_code(LuaByteCode::GetTabUp(0, 0, 0));

        // visit all arguments
        for arg in call.arguments_mut() {
            self.visit_expression_mut(arg);
            let arg_value_index = self.top_attribute().constant_index;
            self.pop_attribute();

            // Load argument
            if let Some(idx) = arg_value_index {
                // TODO:
                // self.push_code(LuaByteCode::LoadK(0, idx as u32));
            }
        }

        self.push_code(LuaByteCode::Call(
            callee_index.unwrap() as u8,
            call.arguments().len() as u8,
            0,
        ))
    }

    fn visit_if_statement_mut(&mut self, ifst: &mut IfStatement) {
        trace!("LuaGen: if statement: {}", ifst.condition());

        let cond_true = self.define_label(Some("if-true"));
        let cond_false = self.define_label(Some("if-false"));

        self.visit_expression_mut(ifst.condition_mut());

        if let Some(then_ctrl) = ifst.then_controlled_mut() {
            self.visit_statement_mut(then_ctrl);
        }
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
                let dest_reg = self
                    .top_attribute()
                    .register
                    .unwrap_or_else(|| self.reg_mgr.alloc_hard());

                self.push_access_attribute(LuaAccessMode::READ);
                self.visit_expression_mut(&mut operands[0]);
                let op0_reg = self.pop_attribute().register.unwrap();

                self.push_access_attribute(LuaAccessMode::READ);
                self.visit_expression_mut(&mut operands[1]);
                let op1_reg = self.pop_attribute().register.unwrap();

                // generate operators
                match op {
                    // a + b
                    Operator::Plus => self.push_code(LuaByteCode::Add(dest_reg, op0_reg, op1_reg)),
                    // a = b
                    Operator::Equal => {
                        // (op0 == op1) != 1
                        self.push_code(LuaByteCode::Eq(op0_reg, op1_reg, 1))
                    }
                    _ => unreachable!(),
                }

                self.reg_mgr.free(&op0_reg);
                self.reg_mgr.free(&op1_reg);
                self.top_attribute().register = Some(dest_reg);
            }

            _ => unreachable!(),
        }
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        trace!("LuaGen: assignment expression: {}", assign);

        self.push_access_attribute(LuaAccessMode::READ);
        assign.right_mut().accept_mut(self);
        let rhs = self.pop_attribute();

        // Get lhs register
        self.push_access_attribute(LuaAccessMode::READ);
        assign.left_mut().accept_mut(self);
        let lhs_reg = self.pop_attribute().register.unwrap();

        if let Some(constant_index) = rhs.constant_index {
            self.code_load_constant(lhs_reg, constant_index)
        } else if let Some(r) = rhs.register {
            assert!(lhs_reg != r);
            self.reg_mgr.free(&r);
            self.code_move(r, lhs_reg);
        } else {
            panic!()
        }

        self.top_attribute().register = Some(lhs_reg);
    }
}
