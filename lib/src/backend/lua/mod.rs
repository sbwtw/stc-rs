/// Lua ByteCode object wrapper
mod bytecode;
use bytecode::{LuaByteCode, LuaCode, LuaConstants};

/// 32-bits Lua instruction bytecode encoding/decoding
mod encoding;

use crate::backend::*;
use crate::parser::{LiteralValue, Operator};
use crate::prelude::*;

use indexmap::IndexSet;
use log::*;
use smallvec::{smallvec, SmallVec};
use std::rc::Rc;

type RegisterId = usize;

#[derive(Clone)]
pub struct LuaBackendAttribute {
    variable: Option<Rc<Variable>>,
    register: Option<RegisterId>,
    constant_index: Option<usize>,
    scope: Option<Scope>,
    error: bool,
    access_mode: AccessModeFlags,
}

impl Default for LuaBackendAttribute {
    fn default() -> Self {
        Self {
            variable: None,
            register: None,
            scope: None,
            error: false,
            access_mode: AccessModeFlags::NONE,
            constant_index: None,
        }
    }
}

pub struct LuaBackend {
    mgr: UnitsManager,
    app: ModuleContext,
    byte_codes: Vec<LuaByteCode>,
    attributes: SmallVec<[LuaBackendAttribute; 32]>,
    local_function: Option<Function>,
    constants: IndexSet<LuaConstants>,
}

impl LuaBackend {
    fn push_attribute_with_scope(&mut self, scope: Scope) {
        let attr = LuaBackendAttribute {
            scope: Some(scope),
            ..Default::default()
        };

        self.attributes.push(attr)
    }

    fn push_access_attribute(&mut self, access: AccessModeFlags) {
        let attr = LuaBackendAttribute {
            scope: self.top_attribute().scope.clone(),
            access_mode: access,
            ..Default::default()
        };

        self.attributes.push(attr)
    }

    fn push_default_attribute(&mut self) {
        let attr = LuaBackendAttribute {
            scope: self.top_attribute().scope.clone(),
            ..Default::default()
        };

        self.attributes.push(attr);
    }

    fn pop_attribute(&mut self) -> LuaBackendAttribute {
        self.attributes.pop().unwrap()
    }

    fn top_attribute(&mut self) -> &mut LuaBackendAttribute {
        self.attributes.last_mut().unwrap()
    }

    fn add_string_constant<S: AsRef<str>>(&mut self, s: S) -> usize {
        let constant = LuaConstants::String(s.as_ref().to_owned());
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx
    }

    fn add_integer_constant(&mut self, i: i64) -> usize {
        let constant = LuaConstants::Integer(i);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx
    }

    fn add_float_constant(&mut self, f: f64) -> usize {
        let constant = LuaConstants::Float(f);
        let (idx, _inserted) = self.constants.insert_full(constant);
        idx
    }
}

impl CodeGenBackend for LuaBackend {
    type Label = usize;

    fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {
            mgr,
            app,
            byte_codes: vec![],
            attributes: smallvec![],
            local_function: None,
            constants: IndexSet::new(),
        }
    }

    fn gen_function(mut self, func: usize) -> Result<Box<dyn TargetCode>, CodeGenError> {
        let f = self
            .app
            .read()
            .get_function(func)
            .ok_or(CodeGenError::FunctionNotDefined(func))?
            .clone();

        self.local_function = Some(f.clone());

        let app_id = self.app.read().id();
        let fun_scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(func));

        let mut fun = f.write();
        self.push_attribute_with_scope(fun_scope);
        self.visit_statement_mut(fun.parse_tree_mut());
        self.pop_attribute();

        Ok(Box::new(LuaCode {
            byte_codes: self.byte_codes,
            constants: self.constants,
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

        match literal.literal() {
            LiteralValue::String(s) => {
                let constant_index = self.add_string_constant(s);
                self.top_attribute().constant_index = Some(constant_index);
            }
            LiteralValue::DInt(i) => {
                let constant_index = self.add_integer_constant(*i as i64);
                self.top_attribute().constant_index = Some(constant_index);
            }
            LiteralValue::Real(s) | LiteralValue::LReal(s) => {
                let f: f64 = s.parse().unwrap();
                let constant_index = self.add_float_constant(f);
                self.top_attribute().constant_index = Some(constant_index);
            }
            _ => {}
        }
    }

    fn visit_variable_expression_mut(&mut self, variable: &mut VariableExpression) {
        trace!("LuaGen: variable expression: {}", variable);

        if self.top_attribute().access_mode == AccessModeFlags::CALL {
            self.top_attribute().constant_index =
                Some(self.add_string_constant(variable.org_name()));
        } else {
            let scope = self.top_attribute().scope.as_ref().unwrap();
            self.top_attribute().variable = scope.find_variable(variable.name());
        }
    }

    fn visit_call_expression_mut(&mut self, call: &mut CallExpression) {
        trace!("LuaGen: call expression: {}", call);

        self.push_access_attribute(AccessModeFlags::CALL);
        self.visit_expression_mut(call.callee_mut());
        let callee_index = self.top_attribute().constant_index;
        self.pop_attribute();
        self.byte_codes.push(LuaByteCode::GetTabUp(0, 0, 0));

        // visit all arguments
        for arg in call.arguments_mut() {
            self.push_access_attribute(AccessModeFlags::PARAMETER);
            self.visit_expression_mut(arg);
            let arg_value_index = self.top_attribute().constant_index;
            self.pop_attribute();

            // Load argument
            if let Some(idx) = arg_value_index {
                let load = LuaByteCode::LoadK(0, idx as u32);
                self.byte_codes.push(load);
            }
        }

        self.byte_codes.push(LuaByteCode::Call(
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
            // binary compare operators
            Operator::Less
            | Operator::LessEqual
            | Operator::Equal
            | Operator::NotEqual
            | Operator::Greater
            | Operator::GreaterEqual => {
                self.push_access_attribute(AccessModeFlags::READ);
                self.visit_expression_mut(&mut operands[0]);
                self.pop_attribute();

                self.push_access_attribute(AccessModeFlags::READ);
                self.visit_expression_mut(&mut operands[1]);
                self.pop_attribute();
            }
            _ => unreachable!(),
        }
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        trace!("LuaGen: assignment expression: {}", assign);
    }
}
