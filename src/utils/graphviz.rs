use crate::ast::{AstVisitor, Expression, OpCode, ExprStatement};
use crate::parser::LiteralType;

pub struct GraphvizExporter;

impl AstVisitor for GraphvizExporter {
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