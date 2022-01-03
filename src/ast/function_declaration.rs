use crate::ast::{AstVisitor, DeclareClass, Type, Variable};
use crate::impl_ast_display;
use crate::parser::StString;
use std::rc::Rc;

#[derive(Debug)]
pub struct FunctionDeclare {
    name: StString,
    decl_class: DeclareClass,
    return_type: Option<Rc<Box<dyn Type>>>,
    parameters: Vec<Rc<Variable>>,
}

impl_ast_display!(FunctionDeclare, visit_function_declaration);

impl FunctionDeclare {
    pub fn new(
        name: StString,
        class: DeclareClass,
        ty: Option<Rc<Box<dyn Type>>>,
        variables: Vec<Rc<Variable>>,
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

    pub fn return_type(&self) -> Option<Rc<Box<dyn Type>>> {
        self.return_type.clone()
    }

    pub fn parameters(&self) -> &Vec<Rc<Variable>> {
        &self.parameters
    }
}
