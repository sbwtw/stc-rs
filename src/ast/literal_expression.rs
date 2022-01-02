use crate::ast::*;
use crate::parser::LiteralValue;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LiteralExpression(LiteralValue);

impl LiteralExpression {
    pub fn new(val: LiteralValue) -> Self {
        LiteralExpression(val)
    }

    pub fn literal(&self) -> &LiteralValue {
        &self.0
    }
}

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

// impl AstNode for LiteralExpression {
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
//
//     fn accept(&self, visitor: &mut dyn AstVisitor) {
//         visitor.visit_literal(&self.0)
//     }
//
//     fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
//         visitor.visit_literal_mut(&mut self.0)
//     }
// }
//
// impl Expression for LiteralExpression {}
