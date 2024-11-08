use crate::impl_ast_display;
use crate::impl_has_attribute;
use crate::prelude::*;
use crate::utils::AttrMap8;
use std::sync::Arc;

#[derive(Debug)]
pub struct FunctionDeclare {
    name: StString,
    decl_class: DeclareClass,
    return_type: Option<Type>,
    parameters: SmallVec8<Arc<Variable>>,
    attributes: AttrMap8,
}

impl_has_attribute!(FunctionDeclare, attributes);
impl_ast_display!(FunctionDeclare, visit_function_declaration);

impl FunctionDeclare {
    pub fn new(
        name: StString,
        class: DeclareClass,
        ty: Option<Type>,
        variables: SmallVec8<Arc<Variable>>,
    ) -> Self {
        Self {
            name,
            decl_class: class,
            return_type: ty,
            parameters: variables,
            attributes: AttrMap8::new(),
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn class(&self) -> &DeclareClass {
        &self.decl_class
    }

    pub fn return_type(&self) -> &Option<Type> {
        &self.return_type
    }

    pub fn parameters(&self) -> &[Arc<Variable>] {
        self.parameters.as_slice()
    }
}
