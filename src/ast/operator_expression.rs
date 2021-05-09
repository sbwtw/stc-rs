use crate::ast::*;

#[derive(Debug)]
pub struct OperatorExpression(OpCode, Vec<Box<dyn Expression>>);

impl OperatorExpression {
    pub fn new(op: OpCode, exprs: Vec<Box<dyn Expression>>) -> Self {
        OperatorExpression(op, exprs)
    }

    pub fn op(&self) -> &OpCode {
        &self.0
    }

    pub fn operands(&self) -> &Vec<Box<dyn Expression>> {
        &self.1
    }
}

impl AstNode for OperatorExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_operator_expression_mut(self)
    }
}

impl Expression for OperatorExpression {}
