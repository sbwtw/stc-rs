use crate::parser::LiteralType;
use std::ops::Deref;
use std::fmt::Debug;

mod expr_statement;

pub use expr_statement::ExprStatement;

pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &LiteralType);
    fn visit_expr(&mut self, expr: &Expr);
    fn visit_expr_statement(&mut self, stmt: &ExprStatement);
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
pub enum Expr {
    Literal(LiteralType),
    Op(Box<Expr>, OpCode, Box<Expr>),
    UnaryOp(OpCode, Box<Expr>),
}

impl AstNode for Expr {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_expr(self)
    }
}

#[derive(Debug)]
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}