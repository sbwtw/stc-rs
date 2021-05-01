use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct VariableExpression(StString);

impl VariableExpression {
    pub fn new(name: StString) -> Self {
        VariableExpression(name)
    }

    pub fn origin_name(&self) -> &String {
        self.0.origin_string()
    }
}

impl AstNode for VariableExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_variable(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_variable(self)
    }
}

impl Expression for VariableExpression {}

