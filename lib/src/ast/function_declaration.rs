use crate::impl_ast_display;
use crate::prelude::*;
use smallvec::SmallVec;
use std::rc::Rc;

#[derive(Debug)]
pub struct FunctionDeclare {
    name: StString,
    #[allow(dead_code)]
    decl_class: DeclareClass,
    return_type: Option<Rc<Box<dyn Type>>>,
    parameters: SmallVec<[Rc<Variable>; 8]>,
}

impl_ast_display!(FunctionDeclare, visit_function_declaration);

impl FunctionDeclare {
    pub fn new(
        name: StString,
        class: DeclareClass,
        ty: Option<Rc<Box<dyn Type>>>,
        variables: SmallVec<[Rc<Variable>; 8]>,
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

    #[allow(dead_code)]
    pub fn class(&self) -> &DeclareClass {
        &self.decl_class
    }

    pub fn return_type(&self) -> Option<Rc<Box<dyn Type>>> {
        self.return_type.clone()
    }

    pub fn parameters(&self) -> &[Rc<Variable>] {
        self.parameters.as_slice()
    }
}
