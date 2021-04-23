use crate::ast::*;

#[derive(Debug)]
pub struct LiteralExpression(pub LiteralType);

impl AstNode for LiteralExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(&self.0)
    }
}

impl Expression for LiteralExpression {}

