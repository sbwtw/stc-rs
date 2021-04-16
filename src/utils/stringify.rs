use crate::ast::{AstNode, AstVisitor, Expr};

pub struct StringifyVisitor;

impl StringifyVisitor {
    pub fn new() -> Self {
        Self {}
    }
}

impl AstVisitor for StringifyVisitor {
    fn visit_expr(&mut self, expr: &Expr) {
        println!("{:?}", expr);
    }
}
