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
    fn visit_literal(&mut self, literal: &mut LiteralType) {
        match literal {
            LiteralType::F32(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::I32(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::U64(x) => write!(self.writer, "{:?}", x).unwrap(),
            LiteralType::String(x) => write!(self.writer, "{:?}", x).unwrap(),
        }
    }

    fn visit_expr_statement(&mut self, stmt: &mut ExprStatement) {
        stmt.expr().accept(self);

        writeln!(self.writer, ";").unwrap();
    }

    fn visit_operator_expression(&mut self, op: &mut OpCode, operands: &mut [Box<dyn Expression>]) {
        match op {
            &mut OpCode::Sub if operands.len() == 1 => {
                self.write_op(op);
                operands[0].accept(self);
            },
            _ => {
                operands[0].accept(self);
                self.write_op(op);
                operands[1].accept(self);
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::*;
    use crate::parser::st::*;
    use crate::utils::*;
    use crate::ast::*;

    #[test]
    fn stringify() {
        let lexer = lexer::Lexer::new("2-3.0/3; -1+\"a\\\"s\\\"d\";");
        let mut r = CompilationUnitsParser::new().parse(lexer).unwrap();

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        r.accept(&mut stringify);

        let buf_str = String::from_utf8_lossy(&buf);
        assert_eq!(buf_str, "2-3.0/3;\n-1+\"a\\\"s\\\"d\";\n");
    }
}