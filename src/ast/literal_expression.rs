use crate::ast::*;

#[derive(Debug)]
pub struct LiteralExpression(LiteralValue);

impl LiteralExpression {
    pub fn new(val: LiteralValue) -> Self {
        LiteralExpression(val)
    }
}

impl AstNode for LiteralExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(&self.0)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_literal_mut(&mut self.0)
    }
}

impl Expression for LiteralExpression {}
