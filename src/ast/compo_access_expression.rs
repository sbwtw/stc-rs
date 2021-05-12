use crate::ast::*;

#[derive(Debug)]
pub struct CompoAccessExpression {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
}

impl CompoAccessExpression {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self { left, right }
    }

    pub fn left(&self) -> &dyn Expression {
        self.left.as_ref()
    }

    pub fn right(&self) -> &dyn Expression {
        self.right.as_ref()
    }
}

impl AstNode for CompoAccessExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_compo_access_expression(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_compo_access_expression(self)
    }
}

impl Expression for CompoAccessExpression {}
