use crate::ast::{AstVisitor, Type};
use crate::impl_ast_display;
use crate::parser::StString;

#[derive(Debug, Clone)]
pub struct VariableExpression {
    name: StString,
    ty: Option<Type>,
}

impl_ast_display!(VariableExpression, visit_variable_expression);

impl VariableExpression {
    pub fn new(var: StString) -> Self {
        Self {
            name: var,
            ty: None,
        }
    }

    #[inline]
    pub fn name(&self) -> &StString {
        &self.name
    }

    /// Origin name of the variable
    #[inline]
    pub fn org_name(&self) -> &String {
        self.name().origin_string()
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
