use crate::ast::{AstVisitorMut, Expression, OpCode, ExprStatement, AstVisitor};
use crate::parser::LiteralType;

pub struct TypeAllocator;

impl AstVisitorMut for TypeAllocator {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType) {
        todo!()
    }

    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement) {
        todo!()
    }

    fn visit_operator_expression_mut(&mut self, op: &mut OpCode, operands: &mut [Box<dyn Expression>]) {
        todo!()
    }
}

impl AstVisitor for TypeAllocator {
    fn visit_literal(&mut self, literal: &LiteralType) {
        todo!()
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        todo!()
    }

    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]) {
        todo!()
    }
}