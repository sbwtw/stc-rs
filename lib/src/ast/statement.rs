use crate::ast::{AstVisitor, ExprStatement, IfStatement};
use crate::impl_ast_display;
use crate::parser::Location;
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
    pub info: StmtInfo,
}

#[derive(Debug, Default)]
pub struct StmtInfo {
    pub start_pos: Option<Location>,
    pub end_pos: Option<Location>,
}

impl_ast_display!(Statement, visit_statement);

impl Statement {
    pub fn update_pos(&mut self, start: Option<Location>, end: Option<Location>) {
        self.info = StmtInfo {
            start_pos: start,
            end_pos: end,
        };
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
            info: StmtInfo::default(),
        }
    }

    #[inline]
    pub fn expr_stmt(
        expr: Box<ExprStatement>,
        start: Option<Location>,
        end: Option<Location>,
    ) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
            info: StmtInfo {
                start_pos: start,
                end_pos: end,
            },
        }
    }

    #[inline]
    pub fn expr(expr: Expression, start: Option<Location>, end: Option<Location>) -> Self {
        Self::expr_stmt(Box::new(ExprStatement::new(expr)), start, end)
    }

    #[inline]
    pub fn if_stmt(
        if_stmt: Box<IfStatement>,
        start: Option<Location>,
        end: Option<Location>,
    ) -> Self {
        Self {
            kind: StmtKind::If(if_stmt),
            info: StmtInfo {
                start_pos: start,
                end_pos: end,
            },
        }
    }
}
