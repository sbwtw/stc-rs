use crate::ast::Type;
use crate::impl_into_expression;
use crate::parser::StString;
use crate::prelude::*;

#[derive(Debug, Clone)]
pub struct VariableExpression {
    name: StString,
    ty: Option<Type>,
}

impl_into_expression!(VariableExpression, |x| Expression::variable(x));
impl_into_expression!(Spanned<VariableExpression>, |x| match x.span {
    Some(loc) => Expression::spanned_variable(x.value, Some(loc.start), Some(loc.end)),
    _ => Expression::variable(x.value),
});

impl VariableExpression {
    pub fn new(var: StString) -> Self {
        Self {
            name: var,
            ty: None,
        }
    }

    /// Origin name of the variable
    #[inline]
    pub fn name(&self) -> &StString {
        &self.name
    }

    #[inline]
    pub fn ty(&self) -> Option<&Type> {
        self.ty.as_ref()
    }

    #[inline]
    pub fn set_ty(&mut self, ty: Option<Type>) {
        self.ty = ty
    }
}
