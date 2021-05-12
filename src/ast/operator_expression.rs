use crate::ast::*;
use crate::parser::Tok;

#[derive(Debug)]
pub struct OperatorExpression {
    op: Tok,
    ty: Option<Box<dyn Type>>,
    exprs: Vec<Box<dyn Expression>>,
}

impl OperatorExpression {
    pub fn new(op: Tok, exprs: Vec<Box<dyn Expression>>) -> Self {
        Self {
            op,
            ty: None,
            exprs,
        }
    }

    pub fn op(&self) -> &Tok {
        &self.op
    }

    pub fn ty(&self) -> Option<&Box<dyn Type>> {
        self.ty.as_ref()
    }

    pub fn operands(&self) -> &Vec<Box<dyn Expression>> {
        &self.exprs
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
