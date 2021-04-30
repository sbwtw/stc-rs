use crate::ast::*;

#[derive(Debug)]
pub struct LiteralExpression(LiteralType);

impl LiteralExpression {
    pub fn new(val: LiteralType) -> Self {
        LiteralExpression(val)
    }
}

impl AstNode for LiteralExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(&self.0)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_literal_mut(&mut self.0)
    }
}

impl Expression for LiteralExpression {}

