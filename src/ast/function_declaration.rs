use crate::ast::*;
use crate::parser::StString;
use std::sync::Arc;

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: StString,
    decl_class: DeclareClass,
    return_type: Option<Box<dyn Type>>,
    variables: Vec<Arc<Variable>>,
}

impl FunctionDeclaration {
    pub fn new(
        name: StString,
        class: DeclareClass,
        ty: Option<Box<dyn Type>>,
        variables: Vec<Arc<Variable>>,
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

    pub fn return_type(&self) -> Option<&Box<dyn Type>> {
        self.return_type.as_ref()
    }

    pub fn variables(&self) -> &Vec<Arc<Variable>> {
        &self.variables
    }
}

impl Declaration for FunctionDeclaration {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn DeclarationVisitor) {
        visitor.visit_function_declare(self)
    }

    fn identifier(&self) -> &StString {
        self.name()
    }
}
