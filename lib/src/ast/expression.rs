use crate::ast::{
    AssignExpression, AstVisitor, AstVisitorMut, CallExpression, CompoAccessExpression,
    ExprStatement, IntoStatement, LiteralExpression, OperatorExpression, Statement,
    VariableExpression, RangeExpression
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
    Range(Box<RangeExpression>),
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
    #[inline]
    pub fn accept_mut<V: AstVisitorMut>(&mut self, vis: &mut V) {
        vis.visit_expression_mut(self)
    }

    #[inline]
    pub fn assign(assign: Box<AssignExpression>) -> Self {
        Self {
            kind: ExprKind::Assign(assign),
        }
    }

    #[inline]
    pub fn call(call: Box<CallExpression>) -> Self {
        Self {
            kind: ExprKind::Call(call),
        }
    }

    #[inline]
    pub fn literal(literal: Box<LiteralExpression>) -> Self {
        Self {
            kind: ExprKind::Literal(literal),
        }
    }

    #[inline]
    pub fn operator(operator: Box<OperatorExpression>) -> Self {
        Self {
            kind: ExprKind::Operator(operator),
        }
    }

    #[inline]
    pub fn variable(variable: Box<VariableExpression>) -> Self {
        Self {
            kind: ExprKind::Variable(variable),
        }
    }

    #[inline]
    pub fn compo(compo: Box<CompoAccessExpression>) -> Self {
        Self {
            kind: ExprKind::Compo(compo),
        }
    }

    #[inline]
    pub fn range(range: Box<RangeExpression>) -> Self {
        Self {
            kind: ExprKind::Range(range),
        }
    }

    #[inline]
    pub fn get_variable_expression(&self) -> Option<&VariableExpression> {
        match &self.kind {
            ExprKind::Variable(var) => Some(var),
            _ => None,
        }
    }
}
