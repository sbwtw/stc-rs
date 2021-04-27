use crate::parser::LiteralType;
use std::fmt::Debug;

mod expr_statement;
mod literal_expression;
mod operator_expression;

pub use expr_statement::ExprStatement;
pub use literal_expression::LiteralExpression;
pub use operator_expression::OperatorExpression;

pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &mut LiteralType);
    fn visit_expr_statement(&mut self, stmt: &mut ExprStatement);
    fn visit_operator_expression(&mut self, op: &mut OpCode, operands: &mut [Box<dyn Expression>]);
}

pub trait AstNode: Debug {
    fn accept(&mut self, visitor: &mut dyn AstVisitor);
}

impl<T> AstNode for Box<T> where T: AstNode {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        self.as_mut().accept(visitor);
    }
}

impl<T> AstNode for Vec<T> where T: AstNode {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
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
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        for x in &mut self.0 {
            x.as_mut().accept(visitor);
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