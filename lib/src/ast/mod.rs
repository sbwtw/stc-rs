use crate::prelude::*;
use bitflags::bitflags;
use smallvec::SmallVec;
use std::any::Any;
use std::fmt::{self, Debug, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Range;
use std::sync::Arc;

mod message;
pub use message::*;

mod types;
pub use types::*;

mod visitor;
pub use visitor::{AstVisitor, AstVisitorMut, DeclVisitor};

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
pub use assign_expression::{AssignExpression, AssignType};

mod compo_access_expression;
pub use compo_access_expression::CompoAccessExpression;

mod statement;
pub use statement::{Statement, StmtKind};

mod expression;
pub use expression::{ExprKind, Expression};

mod variable_expression;
pub use variable_expression::VariableExpression;

mod call_expression;
pub use call_expression::CallExpression;

mod global_variable_declaration;
pub use global_variable_declaration::GlobalVariableDeclare;

mod range_expression;
pub use range_expression::{Dimensions, RangeExpression};

pub type SmallVec8<T> = SmallVec<[T; 8]>;
pub type SmallVec3<T> = SmallVec<[T; 3]>;

pub trait HasSourcePosition {}

#[macro_export]
macro_rules! impl_ast_display {
    ($ty:ident, $fun:ident) => {
        impl std::fmt::Display for $ty {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                let mut buf = vec![];
                let mut stringify = $crate::utils::StringifyVisitor::new(&mut buf);
                stringify.$fun(self);

                write!(f, "{}", String::from_utf8_lossy(&buf))
            }
        }
    };
}

// pub trait IntoStatement {
//     fn into_statement(self) -> Statement;
// }
//
// #[macro_export]
// macro_rules! impl_into_statement {
//     ($ty:ident, $closure:expr) => {
//         impl IntoStatement for $ty {
//             fn into_statement(self) -> Statement {
//                 let f: &dyn Fn(Self) -> Statement = &$closure;
//                 f(self)
//             }
//         }
//     };
// }

// #[macro_export]
// macro_rules! impl_accept {
//     ($ty:ident, $closure:expr) => {
//         impl Accept for $ty {
//             fn accept(&self) {
//                 let f: &dyn Fn(Self) -> Statement = &$closure;
//                 f(self)
//             }
//         }
//     };
// }

pub trait IntoExpression {
    fn into_expression(self) -> Expression;
}

#[macro_export]
macro_rules! impl_into_expression {
    ($ty:ty, $closure:expr) => {
        impl IntoExpression for $ty {
            fn into_expression(self) -> Expression {
                let f: &dyn Fn(Self) -> Expression = &$closure;
                f(self)
            }
        }
    };
}

/// Location range
pub type LocSpan = Range<TokLoc>;
#[derive(Debug, Clone)]
pub struct Spanned<T> {
    pub span: Option<LocSpan>,
    pub value: T,
}

impl<T> Spanned<T> {
    pub fn hint(&self, pos: &TokLoc) -> bool {
        false
    }
}

pub trait TypeTrait: Send + Sync {
    fn class(&self) -> TypeClass;
    fn as_any(&self) -> &dyn Any;
}

pub trait UserTypeTrait: TypeTrait {
    fn get_prototype(&self, scope: Scope) -> Prototype;
}

pub trait ArrayTypeTrait: TypeTrait {
    fn base_type(&self) -> &Type;
    fn dimensions(&self) -> &Dimensions;
}

#[derive(Clone)]
pub struct Type {
    inner: Arc<TypeEnum>,
}

pub enum TypeEnum {
    Basic(TypeClass),
    Complex(Box<dyn TypeTrait>),
}

impl Debug for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{}", self.type_class()))
    }
}

impl Type {
    pub fn from_class(class: TypeClass) -> Self {
        Self {
            inner: Arc::new(TypeEnum::Basic(class)),
        }
    }

    pub fn from_object(o: impl TypeTrait + 'static) -> Self {
        Self {
            inner: Arc::new(TypeEnum::Complex(Box::new(o))),
        }
    }

    pub fn type_class(&self) -> TypeClass {
        match self.inner.as_ref() {
            TypeEnum::Basic(basic) => *basic,
            TypeEnum::Complex(complex) => complex.class(),
        }
    }

    pub fn complex(&self) -> bool {
        matches!(*self.inner, TypeEnum::Complex(..))
    }
}

impl<T> From<T> for Type
where
    T: TypeTrait + 'static,
{
    fn from(value: T) -> Self {
        Type::from_object(value)
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_class())
    }
}

impl PartialEq for Type {
    fn eq(&self, other: &Self) -> bool {
        self.type_class() == other.type_class()
    }
}

impl Eq for Type {}

#[derive(Debug)]
struct TypeInner {
    class: TypeClass,
}

impl PartialEq for TypeInner {
    fn eq(&self, other: &Self) -> bool {
        self.class == other.class
    }
}

impl Eq for TypeInner {}

impl Display for TypeInner {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_fmt(format_args!("{:?}", self.class))
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct CompilePassFlags: u32 {
        const NONE                  = 0b0000_0000_0000_0000;

        // HasError
        const HAS_ERROR             = 0b0001_0000_0000_0001;
        // TypeChecked
        const TYPE_CHECKED          = 0b0000_0000_0000_0010;
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct FunctionFlags: u32 {
        const NONE                  = 0b0000_0000_0000_0000;

        // This Function generated by compiler
        const COMPILER_GENERATED    = 0b0000_0000_0000_0001;
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
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
        const RETAINPERSISTENT  = Self::RETAIN.bits() | Self::PERSISTENT.bits();

        // CV
        const CONST             = 0b0000_1000_0000_0000;

        // special field
        const ENUM_FIELD        = 0b0001_0000_0000_0000;
        const UNION_FIELD       = 0b0010_0000_0000_0000;
    }
}

impl Display for VariableFlags {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "VAR")?;

        // scope
        if self.contains(Self::GLOBAL) {
            write!(f, "_GLOBAL")?;
        } else if self.contains(Self::INPUT) {
            write!(f, "_INPUT")?;
        } else if self.contains(Self::OUTPUT) {
            write!(f, "_OUTPUT")?;
        } else if self.contains(Self::INOUT) {
            write!(f, "_INOUT")?;
        } else if self.contains(Self::TEMP) {
            write!(f, "_TEMP")?;
        } else if self.contains(Self::STATIC) {
            write!(f, "_STAT")?;
        }

        if self.contains(Self::RETAIN) {
            write!(f, " RETAIN")?;
        }

        if self.contains(Self::PERSISTENT) {
            write!(f, " PERSISTENT")?;
        }

        Ok(())
    }
}

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct CompilerInternalFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const HAS_ERROR         = 0b0000_0000_0000_0001;
    }
}

#[derive(Debug)]
pub enum DeclareClass {
    Function,
    Program,
    FunctionBlock,
    Method,
    Action,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UserTypeClass {
    Alias,
    Enum,
    Struct,
    Union,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
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
    /// UnknownType
    UnknownType,
    /// ArrayType
    Array,
    /// StructType
    Struct,
}

impl Hash for TypeClass {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let tag = match self {
            TypeClass::Bit => 1,
            TypeClass::Bool => 2,
            TypeClass::Byte => 3,
            TypeClass::UInt => 4,
            TypeClass::Int => 5,
            TypeClass::UnknownType => 6,
            TypeClass::Array => 7,
            // Some type shouldn't hash directly like ArrayType or UserType
            _ => unreachable!("TypeClass shouldn't hash: {:?}", self),
        };

        tag.hash(state);
    }
}

impl Display for TypeClass {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            TypeClass::Bit => write!(f, "BIT"),
            TypeClass::Bool => write!(f, "BOOL"),
            TypeClass::SInt => write!(f, "SINT"),
            TypeClass::Byte => write!(f, "BYTE"),
            TypeClass::Int => write!(f, "INT"),
            TypeClass::UInt => write!(f, "UINT"),
            TypeClass::DInt => write!(f, "DINT"),
            TypeClass::UDInt => write!(f, "UDINT"),
            TypeClass::LInt => write!(f, "LINT"),
            TypeClass::ULInt => write!(f, "ULINT",),
            TypeClass::Real => write!(f, "REAL"),
            TypeClass::LReal => write!(f, "LREAL"),
            TypeClass::String => write!(f, "STRING"),
            TypeClass::UnknownType | TypeClass::Array | TypeClass::Struct => {
                unreachable!("UserType or ArrayType can't display without Type object")
            }
        }
    }
}
