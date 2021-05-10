use std::fmt::{self, Debug, Display, Formatter};

use crate::parser::{LiteralType, StString, Tok};
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
pub use variable::Variable;

mod assign_expression;
pub use assign_expression::AssignExpression;

mod compo_access_expression;
pub use compo_access_expression::CompoAccessExpression;

mod function_declaration;
pub use function_declaration::FunctionDeclaration;

pub trait Type: Debug {}

pub trait AsAstNode {
    fn as_ast_node(&self) -> &dyn AstNode;
}

impl<T: AstNode> AsAstNode for T {
    fn as_ast_node(&self) -> &dyn AstNode {
        self
    }
}

pub trait AstNode: Debug + AsAstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor);
    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut);
}

impl Display for &dyn AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        self.accept(&mut stringify);

        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl<T> AstNode for Box<T>
where
    T: AstNode,
{
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        self.as_ref().accept(visitor);
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        self.as_mut().accept_mut(visitor);
    }
}

pub trait Statement: AstNode {}

pub trait Expression: AstNode {}

pub trait Declaration: AstNode {}

#[derive(Debug)]
pub struct StatementList(pub Vec<Box<dyn Statement>>);

impl AstNode for StatementList {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_statement_list(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_statement_list(self)
    }
}

impl Statement for StatementList {}

#[derive(Debug)]
pub enum FunctionClass {
    Function,
    Program,
    FunctionBlock,
    Method,
}

#[derive(Debug)]
pub enum UserTypeClass {
    Alias,
    Enum,
    Struct,
    Union,
}

#[derive(Debug)]
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
    /// 'LINT', 64 bits signed
    LInt,
    /// 'ULINT', 64 bits unsigned
    ULInt,
    /// UserType
    UserType(StString),
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
