use crate::ast::{AstVisitor, Expr};
use std::io::Write;

pub struct StringifyVisitor<W: Write> {
    writer: W,
}

impl<W: Write> StringifyVisitor<W> {
    pub fn new(w: W) -> Self {
        Self {writer: w}
    }
}

impl<W: Write> AstVisitor for StringifyVisitor<W> {
    fn visit_expr(&mut self, expr: &Expr) {
        writeln!(self.writer, "{:?}", expr).unwrap();
    }
}
