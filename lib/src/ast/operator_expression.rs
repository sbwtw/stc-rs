use crate::ast::*;
use crate::parser::Operator;
use crate::{impl_ast_display, impl_into_expression};

#[derive(Debug)]
pub struct OperatorExpression {
    op: Operator,
    ty: Option<Type>,
    operands: SmallVec<[Expression; 2]>,
}

impl_ast_display!(OperatorExpression, visit_operator_expression);
impl_into_expression!(OperatorExpression, |x| Expression::operator(Box::new(x)));

impl OperatorExpression {
    pub fn new(op: Operator, operands: SmallVec<[Expression; 2]>) -> Self {
        match operands.len() {
            1 => debug_assert!(op.is_unary_operator(), "'{:?}' is not a unary operator", op),
            2 => debug_assert!(
                op.is_binary_operator(),
                "'{:?}' is not a binary operator",
                op
            ),
            _ => {}
        }

        Self {
            op,
            ty: None,
            operands,
        }
    }

    pub fn op(&self) -> &Operator {
        &self.op
    }

    pub fn ty(&self) -> Option<&Type> {
        self.ty.as_ref()
    }

    pub fn set_ty(&mut self, ty: Option<Type>) {
        self.ty = ty;
    }

    pub fn operands(&self) -> &[Expression] {
        &self.operands
    }

    pub fn operands_mut(&mut self) -> &mut [Expression] {
        self.operands.as_mut()
    }
}

// impl AstNode for OperatorExpression {
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
//
//     fn accept(&self, visitor: &mut dyn AstVisitor) {
//         visitor.visit_operator_expression(self)
//     }
//
//     fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
//         visitor.visit_operator_expression_mut(self)
//     }
// }
//
// impl Expression for OperatorExpression {}
