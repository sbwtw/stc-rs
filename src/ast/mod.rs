use crate::parser::LiteralType;
use std::ops::Deref;
use std::fmt::Debug;

mod expr_statement;

pub use expr_statement::ExprStatement;

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
pub struct LiteralExpression(pub LiteralType);

impl AstNode for LiteralExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(&self.0)
    }
}

impl Expression for LiteralExpression {}

#[derive(Debug)]
pub struct OperatorExpression(pub OpCode, pub Vec<Box<dyn Expression>>);

impl AstNode for OperatorExpression {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_operator_expression(&self.0, &self.1)
    }
}

impl Expression for OperatorExpression {}

#[derive(Debug)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}