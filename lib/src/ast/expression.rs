use crate::ast::{
    AssignExpression, AstVisitor, CallExpression, CompoAccessExpression, ExprStatement,
    IntoStatement, LiteralExpression, OperatorExpression, Statement, VariableExpression,
};
use crate::{impl_ast_display, impl_into_statement};

#[derive(Debug)]
pub enum ExprKind {
    Assign(Box<AssignExpression>),
    Literal(Box<LiteralExpression>),
    Operator(Box<OperatorExpression>),
    Variable(Box<VariableExpression>),
    Compo(Box<CompoAccessExpression>),
    Call(Box<CallExpression>),
}

#[derive(Debug)]
pub struct Expression {
    pub kind: ExprKind,
}

impl_ast_display!(Expression, visit_expression);
impl_into_statement!(Expression, |x| Statement::expr(Box::new(
    ExprStatement::new(x)
)));

impl Expression {
    pub fn assign(assign: Box<AssignExpression>) -> Self {
        Self {
            kind: ExprKind::Assign(assign),
        }
    }

    pub fn call(call: Box<CallExpression>) -> Self {
        Self {
            kind: ExprKind::Call(call),
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
