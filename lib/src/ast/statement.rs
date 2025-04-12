use crate::ast::{AstVisitor, ExprStatement, IfStatement};
use crate::impl_ast_display;
use crate::parser::TokLoc;
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
    pub info: Option<LocSpan>,
}

impl_ast_display!(Statement, visit_statement);

impl Statement {
    pub fn update_pos(&mut self, start: Option<TokLoc>, end: Option<TokLoc>) {
        self.info = match (start, end) {
            (Some(start), Some(end)) => Some(LocSpan { start, end }),
            (None, None) => None,
            _ => panic!("missing fields on pos"),
        }
    }

    pub fn push(self, stmt: Statement) -> Self {
        if let StmtKind::Stmts(mut stmts) = self.kind {
            stmts.push(stmt);
            return Self::statement_list(stmts);
        }

        Self::statement_list(Box::new(vec![self, stmt]))
    }

    #[inline]
    pub fn statement_list(stmts: Box<Vec<Statement>>) -> Self {
        Self {
            kind: StmtKind::Stmts(stmts),
            info: None,
        }
    }

    #[inline]
    pub fn expr_stmt(expr: Box<ExprStatement>, start: Option<TokLoc>, end: Option<TokLoc>) -> Self {
        let mut s = Self {
            kind: StmtKind::Expr(expr),
            info: None,
        };

        s.update_pos(start, end);
        s
    }

    #[inline]
    pub fn expr(expr: Expression, start: Option<TokLoc>, end: Option<TokLoc>) -> Self {
        Self::expr_stmt(Box::new(ExprStatement::new(expr)), start, end)
    }

    #[inline]
    pub fn if_stmt(if_stmt: Box<IfStatement>, start: Option<TokLoc>, end: Option<TokLoc>) -> Self {
        let mut s = Self {
            kind: StmtKind::If(if_stmt),
            info: None,
        };

        s.update_pos(start, end);
        s
    }
}
