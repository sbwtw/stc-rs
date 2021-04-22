use crate::ast::*;
use std::io::Write;
use crate::parser::LiteralType;

pub struct StringifyVisitor<W: Write> {
    writer: W,
}

impl<W: Write> StringifyVisitor<W> {
    pub fn new(w: W) -> Self {
        Self {writer: w}
    }

    fn write_op(&mut self, op: &OpCode) {
        match op {
            OpCode::Add => write!(self.writer, "+").unwrap(),
            OpCode::Sub => write!(self.writer, "-").unwrap(),
            OpCode::Div => write!(self.writer, "/").unwrap(),
            OpCode::Mul => write!(self.writer, "*").unwrap(),
        }
    }
}

impl<W: Write> AstVisitor for StringifyVisitor<W> {
    fn visit_literal(&mut self, literal: &LiteralType) {
        match literal {
            LiteralType::F32(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::I32(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::U64(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::String(x) => write!(self.writer, "{:?}", x).unwrap(),
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Literal(literal) => literal.accept(self),
            Expr::Op(lhs, op, rhs) => {
                lhs.accept(self);
                self.write_op(op);
                rhs.accept(self);
            }
            Expr::UnaryOp(op, expr) => {
                self.write_op(op);
                expr.accept(self);
            }
        }
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        stmt.0.accept(self);

        // match stmt {
        //     Statement::ExprStatement(expr) => expr.accept(self),
        // }
        //
        writeln!(self.writer, ";").unwrap();
    }
}
