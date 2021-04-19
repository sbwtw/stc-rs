use crate::parser::LiteralType;
use std::ops::Deref;
use std::fmt::Debug;

pub trait AstVisitor {
    fn visit_expr(&mut self, _expr: &Expr);
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

#[derive(Debug)]
pub enum Statement {
    ExprStatement(Box<Expr>),
}

impl AstNode for Statement {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        match self {
            Statement::ExprStatement(expr) => expr.accept(visitor),
        }
    }
}

#[derive(Debug)]
pub struct StatementList(pub Vec<Statement>);

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
