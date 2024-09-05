use crate::ast::*;
use crate::parser::TokenKind;
use crate::{impl_ast_display, impl_into_expression};

#[derive(Debug)]
pub enum AssignType {
    /// :=
    Assign,
    /// =>
    AssignRight,
    /// S=
    Set,
    /// R=
    Reset,
}

impl Display for AssignType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match *self {
            AssignType::Assign => f.write_fmt(format_args!("{}", TokenKind::Assign)),
            AssignType::AssignRight => f.write_fmt(format_args!("{}", TokenKind::AssignRight)),
            _ => unreachable!(),
        }
    }
}

#[derive(Debug)]
pub struct AssignExpression {
    left: Expression,
    right: Expression,
    assign_type: AssignType,
    ty: Option<Type>,
}

impl_ast_display!(AssignExpression, visit_assign_expression);
impl_into_expression!(AssignExpression, |x| Expression::assign(Box::new(x)));

impl AssignExpression {
    pub fn new(lhs: Expression, rhs: Expression) -> Self {
        Self {
            left: lhs,
            right: rhs,
            assign_type: AssignType::Assign,
            ty: None,
        }
    }

    pub fn with_type(lhs: Expression, rhs: Expression, aty: AssignType) -> Self {
        Self {
            left: lhs,
            right: rhs,
            assign_type: aty,
            ty: None,
        }
    }

    pub fn assign_type(&self) -> &AssignType {
        &self.assign_type
    }

    #[inline]
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

    pub fn ty(&self) -> Option<&Type> {
        self.ty.as_ref()
    }

    pub fn set_ty(&mut self, ty: Option<Type>) {
        self.ty = ty
    }
}
