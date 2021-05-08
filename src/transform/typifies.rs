use crate::ast::{AstVisitor, AstVisitorMut, ExprStatement, Expression, IfStatement, OpCode, StatementList, VariableExpression, AssignExpression};
use crate::parser::LiteralType;

pub struct TypeAllocator;

impl AstVisitorMut for TypeAllocator {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType) {
        todo!()
    }

    fn visit_variable_mut(&mut self, variable: &mut VariableExpression) {
        todo!()
    }

    fn visit_statement_list_mut(&mut self, stmt: &mut StatementList) {
        todo!()
    }

    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement) {
        todo!()
    }

    fn visit_if_statement_mut(&mut self, stmt: &mut IfStatement) {
        todo!()
    }

    fn visit_operator_expression_mut(
        &mut self,
        op: &mut OpCode,
        operands: &mut [Box<dyn Expression>],
    ) {
        todo!()
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        todo!()
    }
}

impl AstVisitor for TypeAllocator {
    fn visit_literal(&mut self, literal: &LiteralType) {
        todo!()
    }

    fn visit_variable(&mut self, variable: &VariableExpression) {
        todo!()
    }

    fn visit_statement_list(&mut self, stmt: &StatementList) {
        todo!()
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        todo!()
    }

    fn visit_if_statement(&mut self, stmt: &IfStatement) {
        todo!()
    }

    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]) {
        todo!()
    }

    fn visit_assign_expression(&mut self, assign: &AssignExpression) {
        todo!()
    }
}
