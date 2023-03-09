use crate::ast::{IfStatement, OperatorExpression, Variable};
use crate::codegen::{CodeGenAdapter, CodeGenBackend, CodeGenError, TargetCode};
use std::fmt::{Display, Formatter};

trait LuaInstruction {}

pub struct LuaCode {}

impl Display for LuaCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl TargetCode for LuaCode {}

pub struct LuaBackend {
    instructions: Vec<Box<dyn LuaInstruction>>,
}

impl LuaBackend {
    pub fn new() -> Self {
        Self {
            instructions: vec![],
        }
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

    fn gen_variable_load(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn gen_operator(&mut self, operator: &mut OperatorExpression) {
        let operands = operator.operands_mut();

        // self.adapter.unwrap().visit_expression_mut(operands[0]);
    }
}
