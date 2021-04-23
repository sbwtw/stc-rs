use crate::parser::LiteralType;
use std::ops::Deref;
use std::fmt::Debug;

mod expr_statement;
mod literal_expression;
mod operator_expression;

pub use expr_statement::ExprStatement;
pub use literal_expression::LiteralExpression;
pub use operator_expression::OperatorExpression;

pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &LiteralType);
    fn visit_expr_statement(&mut self, stmt: &ExprStatement);
    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]);
}

pub trait AstNode: Debug {
    fn accept(&self, visitor: &mut dyn AstVisitor);
}

impl<T> AstNode for Box<T> where T: AstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        self.deref().accept(visitor);
    }
}

impl<T> AstNode for Vec<T> where T: AstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        for x in self {
            x.accept(visitor);
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
}

#[derive(Debug)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}