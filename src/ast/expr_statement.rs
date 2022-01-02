use crate::ast::*;

#[derive(Debug)]
pub struct ExprStatement(Expression);

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

//
// impl AstNode for ExprStatement {
//     fn as_any(&self) -> &dyn Any {
//         self
//     }
//
//     fn accept<'a, V: AstVisitor<'a>>(&self, visitor: &mut V) {
//         visitor.visit_expr_statement(self)
//     }
//
//     fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
//         visitor.visit_expr_statement_mut(self)
//     }
// }
//
// impl Statement for ExprStatement {}
