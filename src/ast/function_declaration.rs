use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: StString,
    fun_class: FunctionClass,
    return_type: Option<Box<dyn Type>>,
    variables: Vec<Variable>,
}

impl FunctionDeclaration {
    pub fn new(
        name: StString,
        fun: FunctionClass,
        ty: Option<Box<dyn Type>>,
        variables: Vec<Variable>,
    ) -> Self {
        Self {
            name,
            fun_class: fun,
            return_type: ty,
            variables,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn class(&self) -> &FunctionClass {
        &self.fun_class
    }

    pub fn return_type(&self) -> Option<&Box<dyn Type>> {
        self.return_type.as_ref()
    }

    pub fn variables(&self) -> &Vec<Variable> {
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
}
