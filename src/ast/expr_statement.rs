use crate::ast::*;

#[derive(Debug)]
pub struct ExprStatement(pub Box<dyn Expression>);

impl AstNode for ExprStatement {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_expr_statement(self)
    }
}

impl Statement for ExprStatement {}
