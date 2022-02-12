use bitflags::bitflags;
use smallvec::SmallVec;
use std::fmt::{self, Debug, Display, Formatter};

use crate::parser::{LiteralValue, StString, Tok};

mod message;
pub use message::*;

mod types;
pub use types::*;

mod visitor;
pub use visitor::{AstVisitor, AstVisitorMut};

mod function_declaration;
pub use function_declaration::FunctionDeclare;

mod expr_statement;
pub use expr_statement::ExprStatement;

mod if_statement;
pub use if_statement::{ElseIfStatement, IfStatement};

mod declaration_statement;
pub use declaration_statement::{DeclKind, Declaration};

mod literal_expression;
pub use literal_expression::LiteralExpression;

mod operator_expression;
pub use operator_expression::OperatorExpression;

mod variable;
pub use variable::{Variable, VariableDeclareGroup};

mod assign_expression;
pub use assign_expression::AssignExpression;

mod compo_access_expression;
pub use compo_access_expression::CompoAccessExpression;

mod statement;
pub use statement::{Statement, StmtKind};

mod expression;
pub use expression::{ExprKind, Expression};

mod variable_expression;
pub use variable_expression::VariableExpression;

mod global_variable_declaration;
pub use global_variable_declaration::GlobalVariableDeclare;

pub type SmallVec8<T> = SmallVec<[T; 8]>;

pub trait HasSourcePosition {}

pub trait HasAttribute {
    fn set_attribute<K: AsRef<StString>, V: Into<String>>(&mut self, k: K, v: V);
    fn get_attribute_value<S: AsRef<StString>>(&self, attr: &S) -> Option<&String>;
    fn remove_attribute<K: AsRef<StString>>(&mut self, k: K) -> Option<String>;
}

#[macro_export]
macro_rules! impl_has_attribute {
    ($ty:ident, $storage:ident) => {
        impl HasAttribute for $ty {
            fn set_attribute<K: AsRef<StString>, V: Into<String>>(&mut self, k: K, v: V) {
                self.$storage.insert(k.as_ref().clone(), v.into());
            }

            fn get_attribute_value<S: AsRef<StString>>(&self, attr: &S) -> Option<&String> {
                self.$storage.get(&attr.as_ref())
            }

            fn remove_attribute<K: AsRef<StString>>(&mut self, k: K) -> Option<String> {
                self.$storage.remove(k.as_ref())
            }
        }
    };
}

#[macro_export]
macro_rules! impl_ast_display {
    ($ty:ident, $fun:ident) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut buf = vec![];
                let mut stringify = crate::utils::StringifyVisitor::new(&mut buf);
                stringify.$fun(self);

                write!(f, "{}", String::from_utf8_lossy(&buf))
            }
        }
    };
}

pub trait IntoStatement {
    fn into_statement(self) -> Statement;
}

#[macro_export]
macro_rules! impl_into_statement {
    ($ty:ident, $closure:expr) => {
        impl IntoStatement for $ty {
            fn into_statement(self) -> Statement {
                let f: &dyn Fn(Self) -> Statement = &$closure;
                f(self)
            }
        }
    };
}

pub trait IntoExpression {
    fn into_expression(self) -> Expression;
}

#[macro_export]
macro_rules! impl_into_expression {
    ($ty:ident, $closure:expr) => {
        impl IntoExpression for $ty {
            fn into_expression(self) -> Expression {
                let f: &dyn Fn(Self) -> Expression = &$closure;
                f(self)
            }
        }
    };
}

impl_into_expression!(LiteralValue, |x| Expression::literal(Box::new(
    LiteralExpression::new(x)
)));

pub trait Type: Debug {
    fn type_class(&self) -> TypeClass;
}

impl Display for dyn Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_class())
    }
}

bitflags! {
    pub struct VariableFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;

        // Scopes
        const GLOBAL            = 0b0000_0000_0000_0001;
        const INPUT             = 0b0000_0000_0000_0010;
        const INOUT             = 0b0000_0000_0000_0100;
        const OUTPUT            = 0b0000_0000_0000_1000;
        const TEMP              = 0b0000_0000_0001_0000;
        const STATIC            = 0b0000_0000_0010_0000;

        // Retain
        const RETAIN            = 0b0000_0001_0000_0000;
        const PERSISTENT        = 0b0000_0010_0000_0000;
        const RETAINPERSISTENT  = Self::RETAIN.bits | Self::PERSISTENT.bits;

        // CV
        const CONST             = 0b0000_1000_0000_0000;

        // special field
        const ENUM_FIELD        = 0b0001_0000_0000_0000;
        const UNION_FIELD       = 0b0010_0000_0000_0000;
    }
}

bitflags! {
    pub struct CompilerInternalFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const HAS_ERROR         = 0b0000_0000_0000_0001;
    }
}

#[derive(Debug)]
#[allow(dead_code)]
pub enum DeclareClass {
    Function,
    Program,
    FunctionBlock,
    Method,
    Action,
}

#[derive(Debug, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum UserTypeClass {
    Alias,
    Enum,
    Struct,
    Union,
}

#[derive(Debug, Eq, PartialEq, Clone)]
#[allow(dead_code)]
pub enum TypeClass {
    /// 'BIT', one bit type
    Bit,
    /// 'BOOL', boolean type
    Bool,
    /// 'SINT', 8 bits signed
    SInt,
    /// 'BYTE', 8 bits unsigned
    Byte,
    /// 'INT', 16 bits signed
    Int,
    /// 'UINT', 16 bits unsigned
    UInt,
    /// 'DINT', 32 bits signed
    DInt,
    /// 'UDINT' 32 bits unsigned
    UDInt,
    /// 'LINT', 64 bits signed
    LInt,
    /// 'ULINT', 64 bits unsigned
    ULInt,
    /// 'REAL', 32 bits float
    Real,
    /// 'LREAL' 64 bits float
    LReal,
    /// 'STRING' string type
    String,
    /// UserType
    UserType(StString, Option<UserTypeClass>),
}

impl From<Tok> for TypeClass {
    fn from(tok: Tok) -> Self {
        match tok {
            Tok::Bit => TypeClass::Bit,
            Tok::Bool => TypeClass::Bool,
            Tok::SInt => TypeClass::SInt,
            Tok::Byte => TypeClass::Byte,
            Tok::Int => TypeClass::Int,
            Tok::UInt => TypeClass::UInt,
            Tok::DInt => TypeClass::DInt,
            Tok::LInt => TypeClass::LInt,
            Tok::ULInt => TypeClass::ULInt,
            _ => unreachable!(),
        }
    }
}

impl Display for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeClass::Bit => write!(f, "{}", Tok::Bit),
            TypeClass::Bool => write!(f, "{}", Tok::Bool),
            TypeClass::SInt => write!(f, "{}", Tok::SInt),
            TypeClass::Byte => write!(f, "{}", Tok::Byte),
            TypeClass::Int => write!(f, "{}", Tok::Int),
            TypeClass::UInt => write!(f, "{}", Tok::UInt),
            TypeClass::DInt => write!(f, "{}", Tok::DInt),
            TypeClass::UDInt => write!(f, "{}", Tok::UDInt),
            TypeClass::LInt => write!(f, "{}", Tok::LInt),
            TypeClass::ULInt => write!(f, "{}", Tok::ULInt),
            TypeClass::Real => write!(f, "{}", Tok::Real),
            TypeClass::LReal => write!(f, "{}", Tok::LReal),
            TypeClass::String => write!(f, "{}", Tok::String),
            TypeClass::UserType(s, _) => write!(f, "{}", s.origin_string()),
        }
    }
}
