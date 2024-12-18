use crate::ast::Type;
use crate::parser::StString;

#[derive(Debug, Clone)]
pub struct VariableExpression {
    name: StString,
    ty: Option<Type>,
}

// impl_ast_display!(VariableExpression, visit_variable_expression);

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
