use crate::ast::*;
use crate::parser::Tok;
use std::rc::Rc;

#[derive(Debug)]
pub struct OperatorExpression {
    op: Tok,
    ty: Option<Rc<Box<dyn Type>>>,
    operands: Vec<Box<dyn Expression>>,
}

impl OperatorExpression {
    pub fn new(op: Tok, operands: Vec<Box<dyn Expression>>) -> Self {
        match operands.len() {
            1 => debug_assert!(op.is_unary_operator(), "'{}' is not a unary operator", op),
            2 => debug_assert!(op.is_binary_operator(), "'{}' is not a binary operator", op),
            _ => debug_assert!(op.is_operator(), "'{}' is not a operator", op),
        }

        Self {
            op,
            ty: None,
            operands,
        }
    }

    pub fn op(&self) -> &Tok {
        &self.op
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Rc<Box<dyn Type>>>) {
        self.ty = ty;
    }

    pub fn operands(&self) -> &Vec<Box<dyn Expression>> {
        &self.operands
    }

    pub fn operands_mut(&mut self) -> &mut [Box<dyn Expression>] {
        self.operands.as_mut()
    }
}

impl AstNode for OperatorExpression {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_operator_expression_mut(self)
    }
}

impl Expression for OperatorExpression {}
