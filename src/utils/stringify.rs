use crate::ast::*;
use std::io::Write;
use crate::parser::LiteralType;
use std::env::var;

pub struct StringifyVisitor<W: Write> {
    writer: W,
    indent: usize,
}

impl<W: Write> StringifyVisitor<W> {
    pub fn new(w: W) -> Self {
        Self {writer: w, indent: 0}
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

    fn visit_variable(&mut self, variable: &VariableExpression) {
        write!(self.writer, "{}", variable.origin_name()).unwrap();
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        stmt.expr().accept(self);

        writeln!(self.writer, ";").unwrap();
    }

    fn visit_if_statement(&mut self, stmt: &IfStatement) {
        write!(self.writer, "IF ").unwrap();
        stmt.condition().accept(self);
        writeln!(self.writer, " THEN").unwrap();
        if let Some(then_controlled) = stmt.then_controlled() {
            then_controlled.accept(self);
        }
        writeln!(self.writer, "END_IF").unwrap();
    }

    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]) {
        match op {
            &OpCode::Sub if operands.len() == 1 => {
                self.write_op(op);
                operands[0].accept(self);
            },
            _ => {
                operands[0].accept(self);
                write!(self.writer, " ").unwrap();
                self.write_op(op);
                write!(self.writer, " ").unwrap();
                operands[1].accept(self);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::*;
    use crate::parser::st::*;
    use crate::ast::*;
    use crate::utils::*;

    #[test]
    fn stringify() {
        let lexer = Lexer::new("2-3.0/3; -1+\"a\\\"s\\\"d\";");
        let r = CompilationUnitsParser::new().parse(lexer).unwrap();

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        r.accept(&mut stringify);

        let buf_str = String::from_utf8_lossy(&buf);
        assert_eq!(buf_str, "2-3.0/3;\n-1+\"a\\\"s\\\"d\";\n");
    }
}