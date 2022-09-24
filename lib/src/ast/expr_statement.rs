use crate::ast::*;
use crate::{impl_ast_display, impl_into_statement};

#[derive(Debug)]
pub struct ExprStatement(Expression);

impl_into_statement!(ExprStatement, |x| Statement::expr(Box::new(x)));
impl_ast_display!(ExprStatement, visit_expr_statement);

impl ExprStatement {
    pub fn new(expr: Expression) -> Self {
        ExprStatement(expr)
    }

    pub fn expr(&self) -> &Expression {
        &self.0
    }

    pub fn expr_mut(&mut self) -> &mut Expression {
        &mut self.0
    }
}
