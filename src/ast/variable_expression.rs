use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct IdentifierExpression(StString);

impl IdentifierExpression {
    pub fn new(name: StString) -> Self {
        IdentifierExpression(name)
    }

    pub fn origin_name(&self) -> &String {
        self.0.origin_string()
    }
}

impl AstNode for IdentifierExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_identifier(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_identifier(self)
    }
}

impl Expression for IdentifierExpression {}
