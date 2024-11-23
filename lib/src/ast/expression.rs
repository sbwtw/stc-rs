use crate::ast::{
    AssignExpression, AstVisitor, AstVisitorMut, CallExpression, CompoAccessExpression,
    LiteralExpression, OperatorExpression, RangeExpression, VariableExpression,
};
use crate::impl_ast_display;
use crate::parser::{LiteralValue, Operator};
use crate::prelude::*;

use smallvec::{smallvec, SmallVec};

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
    pub(crate) kind: ExprKind,
}

impl_ast_display!(Expression, visit_expression);
// impl_into_statement!(Expression, |x| Statement::expr(Box::new(
//     ExprStatement::new(x)
// )));

impl Expression {
    #[inline]
    pub fn accept_mut<V: AstVisitorMut>(&mut self, vis: &mut V) {
        vis.visit_expression_mut(self)
    }

    /// Get type of this expression
    #[inline]
    pub fn ty(&self) -> Option<&Type> {
        match &self.kind {
            ExprKind::Variable(var_expr) => var_expr.ty(),
            ExprKind::Assign(assign_expr) => assign_expr.ty(),
            ExprKind::Operator(op_expr) => op_expr.ty(),
            _ => None,
        }
    }

    #[inline]
    pub fn assign(assign: Box<AssignExpression>) -> Self {
        Self {
            kind: ExprKind::Assign(assign),
        }
    }

    #[inline]
    pub fn new_assign(lhs: Expression, rhs: Expression) -> Self {
        Self::assign(Box::new(AssignExpression::new(lhs, rhs)))
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
    pub fn new_literal(val: LiteralValue) -> Self {
        Self::literal(Box::new(LiteralExpression::new(val)))
    }

    #[inline]
    pub fn operator(operator: Box<OperatorExpression>) -> Self {
        Self {
            kind: ExprKind::Operator(operator),
        }
    }

    #[inline]
    pub fn new_operator(op: Operator, operands: SmallVec<[Expression; 2]>) -> Self {
        Self::operator(Box::new(OperatorExpression::new(op, operands)))
    }

    #[inline]
    pub fn new_operator2(op: Operator, op1: Expression, op2: Expression) -> Self {
        Self::operator(Box::new(OperatorExpression::new(op, smallvec![op1, op2])))
    }

    #[inline]
    pub fn variable(variable: Box<VariableExpression>) -> Self {
        Self {
            kind: ExprKind::Variable(variable),
        }
    }

    #[inline]
    pub fn new_variable(var: StString) -> Self {
        Self::variable(Box::new(VariableExpression::new(var)))
    }

    #[inline]
    pub fn compo(compo: Box<CompoAccessExpression>) -> Self {
        Self {
            kind: ExprKind::Compo(compo),
        }
    }

    #[inline]
    pub fn new_compo(left: Expression, right: Expression) -> Self {
        Self::compo(Box::new(CompoAccessExpression::new(left, right)))
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
