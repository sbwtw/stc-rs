use crate::ast::*;

#[derive(Debug)]
pub struct OperatorExpression(OpCode, Vec<Box<dyn Expression>>);

impl OperatorExpression {
    pub fn new(op: OpCode, exprs: Vec<Box<dyn Expression>>) -> Self {
        OperatorExpression(op, exprs)
    }
}

impl AstNode for OperatorExpression {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(&mut self.0, &mut self.1)
    }
}

impl Expression for OperatorExpression {}

