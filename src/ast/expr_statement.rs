use crate::ast::*;

#[derive(Debug)]
pub struct ExprStatement(Box<dyn Expression>);

impl ExprStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        ExprStatement(expr)
    }

    pub fn expr(&self) -> &dyn Expression {
        self.0.as_ref()
    }
}

impl AstNode for ExprStatement {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_expr_statement(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_expr_statement_mut(self)
    }
}

impl Statement for ExprStatement {}
