use std::fmt::{self, Debug, Display, Formatter};

use crate::parser::LiteralType;
use crate::utils::StringifyVisitor;

mod expr_statement;
pub use expr_statement::ExprStatement;

mod literal_expression;
pub use literal_expression::LiteralExpression;

mod operator_expression;
pub use operator_expression::OperatorExpression;

mod if_statement;
pub use if_statement::IfStatement;

// Immutable visitor
pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &LiteralType);
    fn visit_expr_statement(&mut self, stmt: &ExprStatement);
    fn visit_if_statement(&mut self, stmt: &IfStatement);
    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]);
}

// Mutable visitor
pub trait AstVisitorMut: AstVisitor {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType);
    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement);
    fn visit_if_statement_mut(&mut self, stmt: &mut IfStatement);
    fn visit_operator_expression_mut(&mut self, op: &mut OpCode, operands: &mut [Box<dyn Expression>]);
}

pub trait AstNode: Debug {
    fn accept(&self, visitor: &mut dyn AstVisitor);
    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut);
}

impl Display for dyn AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        self.accept(&mut stringify);

        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl<T> AstNode for Box<T> where T: AstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        self.as_ref().accept(visitor);
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        self.as_mut().accept_mut(visitor);
    }
}

impl<T> AstNode for Vec<T> where T: AstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        for x in self {
            x.accept(visitor);
        }
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        for x in self {
            x.accept_mut(visitor);
        }
    }
}

pub trait Statement: AstNode {

}

pub trait Expression: AstNode {

}

#[derive(Debug)]
pub struct StatementList(pub Vec<Box<dyn Statement>>);

impl AstNode for StatementList {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        for x in &self.0 {
            x.accept(visitor);
        }
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        for x in &mut self.0 {
            x.accept_mut(visitor);
        }
    }
}

#[derive(Debug)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}
