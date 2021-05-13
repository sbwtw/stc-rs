use crate::ast::*;
use crate::parser::Tok;
use std::sync::Arc;

#[derive(Debug)]
pub struct OperatorExpression {
    op: Tok,
    ty: Option<Arc<Box<dyn Type>>>,
    operands: Vec<Box<dyn Expression>>,
}

impl OperatorExpression {
    pub fn new(op: Tok, operands: Vec<Box<dyn Expression>>) -> Self {
        Self {
            op,
            ty: None,
            operands,
        }
    }

    pub fn op(&self) -> &Tok {
        &self.op
    }

    pub fn ty(&self) -> Option<Arc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Arc<Box<dyn Type>>>) {
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
