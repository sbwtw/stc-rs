use crate::impl_ast_display;
use crate::prelude::*;
use std::rc::Rc;

#[derive(Debug)]
pub struct FunctionDeclare {
    name: StString,
    decl_class: DeclareClass,
    return_type: Option<Type>,
    parameters: SmallVec8<Rc<Variable>>,
}

impl_ast_display!(FunctionDeclare, visit_function_declaration);

impl FunctionDeclare {
    pub fn new(
        name: StString,
        class: DeclareClass,
        ty: Option<Type>,
        variables: SmallVec8<Rc<Variable>>,
    ) -> Self {
        Self {
            name,
            decl_class: class,
            return_type: ty,
            parameters: variables,
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

    pub fn parameters(&self) -> &[Rc<Variable>] {
        self.parameters.as_slice()
    }
}
