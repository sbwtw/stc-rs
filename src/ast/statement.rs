use crate::ast::{AstVisitor, DeclarationStatement, ExprStatement, IfStatement};
use crate::utils::StringifyVisitor;
use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum StmtKind {
    Expr(Box<ExprStatement>),
    If(Box<IfStatement>),
    Stmts(Box<Vec<Statement>>),
    Decl(Box<DeclarationStatement>),
}

#[derive(Debug)]
pub struct Statement {
    pub kind: StmtKind,
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        stringify.visit_statement(self);

        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl Statement {
    pub fn statement_list(stmts: Box<Vec<Statement>>) -> Self {
        Self {
            kind: StmtKind::Stmts(stmts),
        }
    }

    pub fn expr(expr: Box<ExprStatement>) -> Self {
        Self {
            kind: StmtKind::Expr(expr),
        }
    }

    pub fn if_stmt(if_stmt: Box<IfStatement>) -> Self {
        Self {
            kind: StmtKind::If(if_stmt),
        }
    }

    pub fn decl(decl: Box<DeclarationStatement>) -> Self {
        Self {
            kind: StmtKind::Decl(decl),
        }
    }
}
