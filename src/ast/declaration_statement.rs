use crate::ast::*;

#[derive(Debug)]
pub enum DeclKind {
    Fun(Box<FunctionDeclare>),
    Alias(Box<AliasDeclare>),
    Struct(Box<StructDeclare>),
    Enum(Box<EnumDeclare>),
}

#[derive(Debug)]
pub struct DeclarationStatement {
    pub kind: DeclKind,
}

impl DeclarationStatement {
    pub fn identifier(&self) -> &StString {
        match self.kind {
            DeclKind::Fun(ref fun) => fun.name(),
            DeclKind::Alias(ref alias) => alias.name(),
            DeclKind::Struct(ref struct_) => struct_.name(),
            DeclKind::Enum(ref enum_) => enum_.name(),
        }
    }

    pub fn fun(fun: Box<FunctionDeclare>) -> Self {
        Self {
            kind: DeclKind::Fun(fun),
        }
    }

    pub fn alias(alias: Box<AliasDeclare>) -> Self {
        Self {
            kind: DeclKind::Alias(alias),
        }
    }

    pub fn struct_(struct_: Box<StructDeclare>) -> Self {
        Self {
            kind: DeclKind::Struct(struct_),
        }
    }

    pub fn enum_(enum_: Box<EnumDeclare>) -> Self {
        Self {
            kind: DeclKind::Enum(enum_),
        }
    }
}

// impl AstNode for DeclarationStatement {
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
//
//     fn accept(&self, visitor: &mut dyn AstVisitor) {
//         visitor.visit_declaration_statement(self)
//     }
//
//     fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
//         visitor.visit_declaration_statement_mut(self)
//     }
// }
