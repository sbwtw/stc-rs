use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct FunctionDeclaration {
    name: StString,
    return_type: Option<TypeClass>,
}

impl FunctionDeclaration {
    pub fn with_name(name: StString) -> Self {
        Self {
            name,
            return_type: None,
        }
    }
}

impl AstNode for FunctionDeclaration {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_function_declaration(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_function_declaration(self)
    }
}

impl Declaration for FunctionDeclaration {}
