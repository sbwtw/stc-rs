use crate::ast::{AstVisitor, Expression, OpCode, ExprStatement, IfStatement};
use crate::parser::LiteralType;
use std::io::Write;

pub struct GraphvizExporter<W: Write> {
    writer: W,
}

impl<W: Write> AstVisitor for GraphvizExporter<W> {
    fn visit_literal(&mut self, literal: &LiteralType) {
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
}