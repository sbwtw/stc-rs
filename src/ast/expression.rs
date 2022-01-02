use crate::ast::{
    AssignExpression, AstVisitor, CompoAccessExpression, LiteralExpression, OperatorExpression,
    VariableExpression,
};
use crate::utils::StringifyVisitor;
use std::fmt::Formatter;

#[derive(Debug)]
pub enum ExprKind {
    Assign(Box<AssignExpression>),
    Literal(Box<LiteralExpression>),
    Operator(Box<OperatorExpression>),
    Variable(Box<VariableExpression>),
    Compo(Box<CompoAccessExpression>),
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
}

impl std::fmt::Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        stringify.visit_expression(self);

        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl Expression {
    pub fn assign(assign: Box<AssignExpression>) -> Self {
        Self {
            kind: ExprKind::Assign(assign),
        }
    }

    pub fn literal(literal: Box<LiteralExpression>) -> Self {
        Self {
            kind: ExprKind::Literal(literal),
        }
    }

    pub fn operator(operator: Box<OperatorExpression>) -> Self {
        Self {
            kind: ExprKind::Operator(operator),
        }
    }

    pub fn variable(variable: Box<VariableExpression>) -> Self {
        Self {
            kind: ExprKind::Variable(variable),
        }
    }

    pub fn compo(compo: Box<CompoAccessExpression>) -> Self {
        Self {
            kind: ExprKind::Compo(compo),
        }
    }
}
