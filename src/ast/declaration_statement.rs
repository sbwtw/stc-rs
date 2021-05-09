use crate::ast::*;

#[derive(Debug)]
pub struct DeclarationStatement(Box<dyn Declaration>);

impl DeclarationStatement {
    pub fn new(decl: Box<dyn Declaration>) -> Self {
        Self(decl)
    }

    pub fn declaration(&self) -> &dyn Declaration {
        self.0.as_ref()
    }
}

impl AstNode for DeclarationStatement {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_declaration_statement(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_declaration_statement(self)
    }
}

impl Statement for DeclarationStatement {}
