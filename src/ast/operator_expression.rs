use crate::ast::*;

#[derive(Debug)]
pub struct OperatorExpression(pub OpCode, pub Vec<Box<dyn Expression>>);

impl AstNode for OperatorExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(&self.0, &self.1)
    }
}

impl Expression for OperatorExpression {}

