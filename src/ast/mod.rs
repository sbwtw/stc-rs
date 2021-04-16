use crate::parser::LiteralType;
use std::ops::Deref;

pub trait AstVisitor {
    fn visit_expr(&mut self, expr: &Expr);
}

pub trait AstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor);
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
pub enum OpCode {
    Mul,
    Div,
    Add,
    Sub,
}
