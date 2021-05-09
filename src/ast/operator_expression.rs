use crate::ast::*;
use crate::parser::Tok;

#[derive(Debug)]
pub struct OperatorExpression(Tok, Vec<Box<dyn Expression>>);

impl OperatorExpression {
    pub fn new(op: Tok, exprs: Vec<Box<dyn Expression>>) -> Self {
        OperatorExpression(op, exprs)
    }

    pub fn op(&self) -> &Tok {
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
