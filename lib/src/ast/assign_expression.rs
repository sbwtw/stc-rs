use crate::ast::*;
use crate::{impl_ast_display, impl_into_expression};
use std::rc::Rc;

#[derive(Debug)]
pub struct AssignExpression {
    left: Expression,
    right: Expression,
    ty: Option<Rc<Box<dyn Type>>>,
}

impl_ast_display!(AssignExpression, visit_assign_expression);
impl_into_expression!(AssignExpression, |x| Expression::assign(Box::new(x)));

impl AssignExpression {
    pub fn new(lhs: Expression, rhs: Expression) -> Self {
        Self {
            left: lhs,
            right: rhs,
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
