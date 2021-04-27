use crate::ast::*;

#[derive(Debug)]
pub struct ExprStatement(Box<dyn Expression>);

impl ExprStatement {
    pub fn new(expr: Box<dyn Expression>) -> Self {
        ExprStatement(expr)
    }

    pub fn expr(&mut self) -> &mut dyn Expression {
        self.0.as_mut()
    }
}

impl AstNode for ExprStatement {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_expr_statement(self)
    }
}

impl Statement for ExprStatement {}
