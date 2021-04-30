use crate::ast::*;

#[derive(Debug)]
pub struct OperatorExpression(OpCode, Vec<Box<dyn Expression>>);

impl OperatorExpression {
    pub fn new(op: OpCode, exprs: Vec<Box<dyn Expression>>) -> Self {
        OperatorExpression(op, exprs)
    }
}

impl AstNode for OperatorExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(&self.0, &self.1)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_operator_expression_mut(&mut self.0, &mut self.1)
    }
}

impl Expression for OperatorExpression {}

