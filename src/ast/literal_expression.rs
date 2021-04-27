use crate::ast::*;

#[derive(Debug)]
pub struct LiteralExpression(LiteralType);

impl LiteralExpression {
    pub fn new(val: LiteralType) -> Self {
        LiteralExpression(val)
    }
}

impl AstNode for LiteralExpression {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(&mut self.0)
    }
}

impl Expression for LiteralExpression {}

