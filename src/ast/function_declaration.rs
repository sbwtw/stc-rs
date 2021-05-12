use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: StString,
    fun_type: FunctionType,
    return_type: Option<Box<dyn Type>>,
    variables: Vec<Variable>,
}

impl FunctionDeclaration {
    pub fn new(
        name: StString,
        fun: FunctionType,
        ty: Option<Box<dyn Type>>,
        variables: Vec<Variable>,
    ) -> Self {
        Self {
            name,
            fun_type: fun,
            return_type: ty,
            variables,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn ty(&self) -> &FunctionType {
        &self.fun_type
    }

    pub fn variables(&self) -> &Vec<Variable> {
        &self.variables
    }
}

impl AstNode for FunctionDeclaration {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_declaration(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_function_declaration(self)
    }
}

impl Declaration for FunctionDeclaration {}
