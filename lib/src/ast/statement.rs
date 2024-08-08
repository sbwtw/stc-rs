use crate::ast::{AstVisitor, ExprStatement, IfStatement, IntoStatement};
use crate::impl_ast_display;
use crate::prelude::*;

#[derive(Debug)]
pub enum StmtKind {
    Expr(Box<ExprStatement>),
    If(Box<IfStatement>),
    Stmts(Box<Vec<Statement>>),
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StmtKind,
}

impl_ast_display!(Statement, visit_statement);

impl Statement {
    pub fn push(self, stmt: Statement) -> Self {
        let x = match self.kind {
            StmtKind::Stmts(mut stmts) => {
                stmts.push(stmt);
                return Self::statement_list(stmts);
            }
            StmtKind::If(x) => x.into_statement(),
            StmtKind::Expr(x) => x.into_statement(),
        };

        Self::statement_list(Box::new(vec![x, stmt]))
    }

    #[inline]
    pub fn statement_list(stmts: Box<Vec<Statement>>) -> Self {
        Self {
            kind: StmtKind::Stmts(stmts),
        }
    }

    #[inline]
    pub fn expr(expr: Box<ExprStatement>) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
        }
    }

    #[inline]
    pub fn new_expr(expr: Expression) -> Self {
        Self::expr(Box::new(ExprStatement::new(expr)))
    }

    #[inline]
    pub fn if_stmt(if_stmt: Box<IfStatement>) -> Self {
        Self {
            kind: StmtKind::If(if_stmt),
        }
    }
}
