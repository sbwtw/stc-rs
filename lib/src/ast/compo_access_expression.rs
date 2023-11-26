use crate::ast::*;
use crate::{impl_ast_display, impl_into_expression};
use std::rc::Rc;

#[derive(Debug)]
pub struct CompoAccessExpression {
    left: Expression,
    right: Expression,
    ty: Option<Rc<Box<dyn Type>>>,
}

impl_ast_display!(CompoAccessExpression, visit_compo_access_expression);
impl_into_expression!(CompoAccessExpression, |x| Expression::compo(Box::new(x)));

impl CompoAccessExpression {
    pub fn new(left: Expression, right: Expression) -> Self {
        Self {
            left,
            right,
            ty: None,
        }
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn left_mut(&mut self) -> &mut Expression {
        &mut self.left
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn right_mut(&mut self) -> &mut Expression {
        &mut self.right
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Rc<Box<dyn Type>>>) {
        self.ty = ty
    }
}
