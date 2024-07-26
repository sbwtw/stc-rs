use crate::ast::*;
use crate::utils::HasAttribute;
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

macro_rules! decl_call {
    (mut $obj:ident, $op:ident, $($args:tt),*) => {
        match $obj.kind {
            DeclKind::Fun(ref mut f) => f.$op($($args),*),
            DeclKind::FB(ref mut f) => f.$op($($args),*),
            DeclKind::Prg(ref mut f) => f.$op($($args),*),
            DeclKind::Struct(ref mut s) => s.$op($($args),*),
            DeclKind::Enum(ref mut e) => e.$op($($args),*),
            DeclKind::GlobalVar(ref mut g) => g.$op($($args),*),
            DeclKind::Alias(ref mut a) => a.$op($($args),*),
        }
    };
    ($obj:ident, $op:ident, $($args:tt),*) => {
        match $obj.kind {
            DeclKind::Fun(ref f) => f.$op($($args),*),
            DeclKind::FB(ref f) => f.$op($($args),*),
            DeclKind::Prg(ref f) => f.$op($($args),*),
            DeclKind::Struct(ref s) => s.$op($($args),*),
            DeclKind::Enum(ref e) => e.$op($($args),*),
            DeclKind::GlobalVar(ref g) => g.$op($($args),*),
            DeclKind::Alias(ref a) => a.$op($($args),*),
        }
    };
    ($obj:ident, $op:ident) => {
        match $obj.kind {
            DeclKind::Fun(ref f) => f.$op(),
            DeclKind::FB(ref f) => f.$op(),
            DeclKind::Prg(ref f) => f.$op(),
            DeclKind::Struct(ref s) => s.$op(),
            DeclKind::Enum(ref e) => e.$op(),
            DeclKind::GlobalVar(ref g) => g.$op(),
            DeclKind::Alias(ref a) => a.$op(),
        }
    };
}

impl HasAttribute for Declaration {
    fn remove_attribute(&mut self, k: &StString) -> Option<Option<String>> {
        decl_call!(mut self, remove_attribute, k)
    }

    fn set_attribute<V: Into<Option<String>>>(&mut self, k: StString, v: V) {
        decl_call!(mut self, set_attribute, k, v)
    }

    fn get_attribute_value(&self, attr: &StString) -> Option<&Option<String>> {
        decl_call!(self, get_attribute_value, attr)
    }
}

impl Declaration {
    pub fn identifier(&self) -> &StString {
        decl_call!(self, name, )
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
