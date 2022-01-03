use crate::ast::Type;
use crate::StString;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct VariableExpression {
    name: StString,
    ty: Option<Rc<Box<dyn Type>>>,
}

impl VariableExpression {
    pub fn new(var: StString) -> Self {
        Self {
            name: var,
            ty: None,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Rc<Box<dyn Type>>>) {
        self.ty = ty
    }
}
