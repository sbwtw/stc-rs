use crate::ast::*;
use crate::parser::LiteralType;
use std::fmt::Arguments;
use std::io::Write;

struct StringifyAttribute {
    sub_expression: bool,
}

impl StringifyAttribute {
    // fn empty() -> Self {
    //     Self {
    //         sub_expression: false,
    //     }
    // }

    fn sub_expression() -> Self {
        Self {
            sub_expression: true,
        }
    }
}

pub struct StringifyVisitor<W: Write> {
    writer: W,
    indent: usize,
    attribute_stack: Vec<StringifyAttribute>,
}

impl<W: Write> StringifyVisitor<W> {
    pub fn new(w: W) -> Self {
        Self {
            writer: w,
            indent: 0,
            attribute_stack: vec![],
        }
    }

    fn write_op(&mut self, op: &OpCode) {
        match op {
            OpCode::Add => write!(self.writer, "+").unwrap(),
            OpCode::Sub => write!(self.writer, "-").unwrap(),
            OpCode::Div => write!(self.writer, "/").unwrap(),
            OpCode::Mul => write!(self.writer, "*").unwrap(),
        }
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            write!(self.writer, "    ").unwrap();
        }
    }

    fn write(&mut self, args: Arguments<'_>) {
        write!(self.writer, "{}", args).unwrap();
    }

    fn writeln(&mut self, args: Arguments<'_>) {
        writeln!(self.writer, "{}", args).unwrap();
    }

    fn top(&self) -> Option<&StringifyAttribute> {
        self.attribute_stack.last()
    }

    fn pop(&mut self) -> StringifyAttribute {
        self.attribute_stack.pop().unwrap()
    }

    fn push(&mut self, attr: StringifyAttribute) {
        self.attribute_stack.push(attr)
    }
}

impl<W: Write> AstVisitor for StringifyVisitor<W> {
    fn visit_literal(&mut self, literal: &LiteralType) {
        match literal {
            LiteralType::F32(x) => self.write(format_args!("{:?}", x)),
            LiteralType::I32(x) => self.write(format_args!("{:?}", x)),
            LiteralType::U64(x) => self.write(format_args!("{:?}", x)),
            LiteralType::String(x) => self.write(format_args!("{:?}", x)),
        }
    }

    fn visit_variable(&mut self, variable: &VariableExpression) {
        self.write(format_args!("{}", variable.origin_name()));
    }

    fn visit_statement_list(&mut self, stmt: &StatementList) {
        for s in &stmt.0 {
            s.accept(self);
        }
    }

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        self.write_indent();
        stmt.expr().accept(self);
        self.writeln(format_args!(";"));
    }

    fn visit_if_statement(&mut self, stmt: &IfStatement) {
        self.write_indent();
        self.write(format_args!("IF "));
        stmt.condition().accept(self);
        self.writeln(format_args!(" THEN"));
        if let Some(then_controlled) = stmt.then_controlled() {
            self.indent += 1;
            then_controlled.accept(self);
            self.indent -= 1;
        }
        self.writeln(format_args!("END_IF"));
    }

    fn visit_operator_expression(&mut self, expr: &OperatorExpression) {
        let sub_expression = self.top().map(|x| x.sub_expression).unwrap_or(false);

        if sub_expression {
            self.write(format_args!("("));
        }

        let op = expr.op().clone();
        let operands = expr.operands();

        match op {
            &OpCode::Sub if operands.len() == 1 => {
                self.write_op(op);

                self.push(StringifyAttribute::sub_expression());
                operands[0].accept(self);
                self.pop();
            }
            _ => {
                assert_eq!(operands.len(), 2);

                self.push(StringifyAttribute::sub_expression());
                operands[0].accept(self);
                self.pop();

                self.write(format_args!(" "));
                self.write_op(op);
                self.write(format_args!(" "));

                self.push(StringifyAttribute::sub_expression());
                operands[1].accept(self);
                self.pop();
            }
        }

        if sub_expression {
            self.write(format_args!(")"));
        }
    }

    fn visit_assign_expression(&mut self, assign: &AssignExpression) {
        assign.left().accept(self);
        self.write(format_args!(" := "));
        assign.right().accept(self);
    }
}

#[cfg(test)]
mod test {
    use crate::parser::st::*;
    use crate::parser::*;
    use crate::utils::*;

    fn parse_string<S: AsRef<str>>(s: S) -> String {
        let lexer = Lexer::new(s.as_ref());
        let r = CompilationUnitsParser::new().parse(lexer).unwrap();

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        r.accept(&mut stringify);

        String::from_utf8_lossy(&buf).into()
    }

    #[test]
    fn stringify() {
        let buf_str = parse_string("2-3.0/3; -1+\"a\\\"s\\\"d\";");

        assert_eq!(buf_str, "2 - (3.0 / 3);\n(-1) + \"a\\\"s\\\"d\";\n");
    }

    #[test]
    fn test_if_else() {
        let buf_str = parse_string("if a - 1 then a + 1; else a - 1; end_if");

        assert_eq!(buf_str, "IF a - 1 THEN\n    a + 1;\nEND_IF\n");
    }

    #[test]
    fn test_assign_expr() {
        let buf_str = parse_string("if a - 1 then a:= a + 1; else a - 1; end_if");

        assert_eq!(buf_str, "IF a - 1 THEN\n    a := a + 1;\nEND_IF\n");
    }

    #[test]
    fn test_sub_expr_parenthesis() {
        let buf_str = parse_string("a * (a + 1);");

        assert_eq!(buf_str, "a * (a + 1);\n");
    }
}
