use crate::ast::*;
use std::rc::Rc;

const EMPTY_VARIABLES: &[Rc<Variable>] = &[];

#[derive(Debug)]
pub enum DeclKind {
    Fun(Box<FunctionDeclare>),
    Prg(Box<FunctionDeclare>),
    FB(Box<FunctionDeclare>),
    Alias(Box<AliasDeclare>),
    Struct(Box<StructDeclare>),
    Enum(Box<EnumDeclare>),
    GlobalVar(Box<GlobalVariableDeclare>),
}

#[derive(Debug)]
pub struct Declaration {
    pub kind: DeclKind,
}

impl HasAttribute for Declaration {
    fn set_attribute<K: AsRef<StString>, V: Into<String>>(&mut self, k: K, v: V) {
        todo!()
    }

    fn get_attribute_value<S: AsRef<StString>>(&self, attr: &S) -> Option<&String> {
        todo!()
    }

    fn remove_attribute<K: AsRef<StString>>(&mut self, k: K) -> Option<String> {
        todo!()
    }
}

impl Declaration {
    pub fn identifier(&self) -> &StString {
        match self.kind {
            DeclKind::Fun(ref fun) => fun.name(),
            DeclKind::FB(ref fun) => fun.name(),
            DeclKind::Prg(ref fun) => fun.name(),
            DeclKind::Alias(ref alias) => alias.name(),
            DeclKind::Struct(ref struct_) => struct_.name(),
            DeclKind::Enum(ref enum_) => enum_.name(),
            DeclKind::GlobalVar(ref gv) => gv.name(),
        }
    }

    pub fn variables(&self) -> &[Rc<Variable>] {
        match self.kind {
            DeclKind::Fun(ref f) => f.parameters(),
            DeclKind::Struct(ref s) => s.variables(),
            DeclKind::Enum(ref e) => e.fields(),
            DeclKind::GlobalVar(ref g) => g.variables(),
            _ => EMPTY_VARIABLES,
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
