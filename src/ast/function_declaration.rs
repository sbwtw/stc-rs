use crate::ast::*;
use crate::parser::StString;
use std::rc::Rc;

#[derive(Debug)]
pub struct FunctionDeclare {
    name: StString,
    decl_class: DeclareClass,
    return_type: Option<Rc<Box<dyn Type>>>,
    variables: Vec<Rc<Variable>>,
}

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
            variables,
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

    pub fn variables(&self) -> &Vec<Rc<Variable>> {
        &self.variables
    }
}

// impl Declaration for FunctionDeclaration {
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
//
//     fn accept(&self, visitor: &mut dyn DeclarationVisitor) {
//         visitor.visit_function_declare(self)
//     }
//
//     fn identifier(&self) -> &StString {
//         self.name()
//     }
// }
