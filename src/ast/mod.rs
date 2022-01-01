use bitflags::bitflags;
use std::fmt::{self, Debug, Display, Formatter};

use crate::parser::{StString, Tok};
use crate::utils::StringifyVisitor;

mod types;
pub use types::*;

mod visitor;
pub use visitor::*;

mod expr_statement;
pub use expr_statement::ExprStatement;

mod if_statement;
pub use if_statement::{ElseIfStatement, IfStatement};

mod declaration_statement;
pub use declaration_statement::DeclarationStatement;

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

mod function_declaration;
pub use function_declaration::FunctionDeclaration;
use std::any::Any;

macro_rules! ast_stringify {
    ($host:expr, $fmt:expr) => {{
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        $host.accept(&mut stringify);

        write!($fmt, "{}", String::from_utf8_lossy(&buf))
    }};
}

pub trait HasSourcePosition {}

pub trait HasAttributes {
    fn set_attribute<K: AsRef<StString>, V: Into<String>>(&mut self, k: K, v: V);
    fn get_attribute_value<S: AsRef<StString>>(&self, attr: &S) -> Option<&String>;
    fn remove_attribute<K: AsRef<StString>>(&mut self, k: K) -> Option<String>;
}

#[macro_export]
macro_rules! has_attribute {
    ($ty:ident, $storage:ident) => {
        impl HasAttributes for $ty {
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

pub trait TypeClone {
    fn clone_boxed(&self) -> Box<dyn Type>;
}

pub trait Type: TypeClone + Debug {
    fn type_class(&self) -> TypeClass;
}

impl<T> TypeClone for T
where
    T: 'static + Type + Clone,
{
    fn clone_boxed(&self) -> Box<dyn Type> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Type> {
    fn clone(&self) -> Self {
        self.clone_boxed()
    }
}

impl Display for dyn Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.type_class())
    }
}

pub trait AsAstNode {
    fn as_ast_node(&self) -> &dyn AstNode;
    fn as_ast_node_mut(&mut self) -> &mut dyn AstNode;
}

impl<T: AstNode> AsAstNode for T {
    fn as_ast_node(&self) -> &dyn AstNode {
        self
    }

    fn as_ast_node_mut(&mut self) -> &mut dyn AstNode {
        self
    }
}

pub trait AstNode: Debug + AsAstNode {
    fn as_any(&self) -> &dyn Any;
    fn accept(&self, visitor: &mut dyn AstVisitor);
    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut);
}

impl Display for &dyn AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        ast_stringify!(self, f)
    }
}

impl<T> AstNode for Box<T>
where
    T: AstNode,
{
    fn as_any(&self) -> &dyn Any {
        self.as_ref().as_any()
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        self.as_ref().accept(visitor);
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        self.as_mut().accept_mut(visitor);
    }
}

pub trait Statement: AstNode {}

pub trait Expression: AstNode {}

#[derive(Debug)]
pub struct StatementList(Vec<Box<dyn Statement>>);

impl StatementList {
    pub fn new(v: Vec<Box<dyn Statement>>) -> Self {
        Self(v)
    }

    pub fn statements(&self) -> &[Box<dyn Statement>] {
        self.0.as_ref()
    }

    pub fn statements_mut(&mut self) -> &mut [Box<dyn Statement>] {
        self.0.as_mut()
    }
}

impl AstNode for StatementList {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_statement_list(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_statement_list_mut(self)
    }
}

impl Statement for StatementList {}

pub trait Declaration: Debug {
    fn as_any(&self) -> &dyn Any;
    fn accept(&self, visitor: &mut dyn DeclarationVisitor);
    fn identifier(&self) -> &StString;
}

impl Display for &dyn Declaration {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        ast_stringify!(self, f)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum VariableScopeClass {
    None,
    Global,
    Input,
    InOut,
    Output,
    Temp,
    Static,
}

bitflags! {
    pub struct VariableAnnotationFlags: usize {
        const NONE              = 0b0000_0000_0000_0000;
        const RETAIN            = 0b0000_0000_0000_0001;
        const PERSISTENT        = 0b0000_0000_0000_0010;
        const RETAINPERSISTENT  = Self::RETAIN.bits | Self::PERSISTENT.bits;
    }
}

#[derive(Debug)]
pub enum DeclareClass {
    Function,
    Program,
    FunctionBlock,
    Method,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UserTypeClass {
    Alias,
    Enum,
    Struct,
    Union,
}

#[derive(Debug, Eq, PartialEq, Clone)]
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
