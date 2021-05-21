use crate::ast::*;
use std::sync::Arc;

#[derive(Debug)]
pub struct CompoAccessExpression {
    left: Box<dyn Expression>,
    right: Box<dyn Expression>,
    ty: Option<Arc<Box<dyn Type>>>,
}

impl CompoAccessExpression {
    pub fn new(left: Box<dyn Expression>, right: Box<dyn Expression>) -> Self {
        Self {
            left,
            right,
            ty: None,
        }
    }

    pub fn left(&self) -> &dyn Expression {
        self.left.as_ref()
    }

    pub fn left_mut(&mut self) -> &mut dyn Expression {
        self.left.as_mut()
    }

    pub fn right(&self) -> &dyn Expression {
        self.right.as_ref()
    }

    pub fn right_mut(&mut self) -> &mut dyn Expression {
        self.right.as_mut()
    }

    pub fn ty(&self) -> Option<Arc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Arc<Box<dyn Type>>>) {
        self.ty = ty
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
        visitor.visit_compo_access_expression_mut(self)
    }
}

impl Expression for CompoAccessExpression {}
