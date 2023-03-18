use crate::ast::{
    AssignExpression, AstVisitorMut, IfStatement, OperatorExpression, Variable, VariableExpression,
};
use crate::codegen::{AccessModeFlags, CodeGenBackend, CodeGenError, TargetCode};
use crate::context::{ModuleContext, Scope, UnitsManager};

use crate::parser::Operator;
use log::*;
use smallvec::{smallvec, SmallVec};
use std::fmt::{Display, Formatter};
use std::rc::Rc;

type RegisterId = usize;

trait LuaInstruction {}

pub struct LuaCode {}

impl Display for LuaCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        unimplemented!()
    }
}

impl TargetCode for LuaCode {}

#[derive(Clone)]
pub struct LuaBackendAttribute {
    variable: Option<Rc<Variable>>,
    register: Option<RegisterId>,
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
        }
    }
}

pub struct LuaBackend {
    mgr: UnitsManager,
    app: ModuleContext,
    instructions: Vec<Box<dyn LuaInstruction>>,
    attributes: SmallVec<[LuaBackendAttribute; 16]>,
}

impl LuaBackend {
    pub fn new(mgr: UnitsManager, app: ModuleContext) -> Self {
        Self {
            mgr,
            app,
            instructions: vec![],
            attributes: smallvec![],
        }
    }

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
}

impl CodeGenBackend for LuaBackend {
    type Label = usize;

    fn take_code(&mut self) -> Box<dyn TargetCode> {
        Box::new(LuaCode {})
    }

    fn define_label<S: AsRef<str>>(&mut self, label: Option<S>) -> Self::Label {
        0
    }

    fn gen_function(&mut self, func: usize) -> Result<(), CodeGenError> {
        let f = self
            .app
            .read()
            .get_function(func)
            .ok_or(CodeGenError::FunctionNotDefined(func))?
            .clone();

        let app_id = self.app.read().id();
        let fun_scope = Scope::new(Some(self.mgr.clone()), Some(app_id), Some(func));

        let mut fun = f.write().unwrap();
        self.push_attribute_with_scope(fun_scope);
        self.visit_statement_mut(fun.body_mut());
        self.pop_attribute();

        Ok(())
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
    fn visit_variable_expression_mut(&mut self, variable: &mut VariableExpression) {
        trace!("LuaGen: variable expression: {}", variable);

        let scope = self.top_attribute().scope.as_ref().unwrap();
        self.top_attribute().variable = scope.find_variable(variable.name());
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
