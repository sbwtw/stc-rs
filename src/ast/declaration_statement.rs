use crate::ast::*;
use std::borrow::Cow;
use std::rc::Rc;

#[derive(Debug)]
pub enum DeclKind {
    // Stmts(Box<Vec<DeclarationStatement>>),
    Fun(Box<FunctionDeclare>),
    Alias(Box<AliasDeclare>),
    Struct(Box<StructDeclare>),
    Enum(Box<EnumDeclare>),
    GlobalVar(Box<GlobalVariableDeclare>),
}

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclKind,
}

impl Declaration {
    pub fn identifier(&self) -> &StString {
        match self.kind {
            DeclKind::Fun(ref fun) => fun.name(),
            DeclKind::Alias(ref alias) => alias.name(),
            DeclKind::Struct(ref struct_) => struct_.name(),
            DeclKind::Enum(ref enum_) => enum_.name(),
            DeclKind::GlobalVar(ref gv) => gv.name(),
        }
    }

    pub fn variables(&self) -> Cow<Vec<Rc<Variable>>> {
        match self.kind {
            DeclKind::Fun(ref f) => Cow::Borrowed(f.parameters()),
            DeclKind::Struct(ref s) => Cow::Borrowed(s.variables()),
            DeclKind::Enum(ref e) => Cow::Borrowed(e.fields()),
            DeclKind::GlobalVar(ref g) => Cow::Borrowed(g.variables()),
            _ => Cow::Owned(vec![]),
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

    pub fn global_var(global_var: Box<GlobalVariableDeclare>) -> Self {
        Self {
            kind: DeclKind::GlobalVar(global_var),
        }
    }
}
