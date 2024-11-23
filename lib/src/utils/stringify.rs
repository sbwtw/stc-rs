use crate::ast::*;
use crate::parser::{BitValue, LiteralValue, Operator, TokenKind};
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

    #[inline]
    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            write!(self.writer, "    ").unwrap();
        }
    }

    #[inline]
    fn write(&mut self, args: Arguments<'_>) {
        write!(self.writer, "{}", args).unwrap();
    }

    #[inline]
    fn writeln(&mut self, args: Arguments<'_>) {
        writeln!(self.writer, "{}", args).unwrap();
    }

    #[inline]
    fn top(&self) -> Option<&StringifyAttribute> {
        self.attribute_stack.last()
    }

    #[inline]
    fn pop(&mut self) -> StringifyAttribute {
        self.attribute_stack.pop().unwrap()
    }

    #[inline]
    fn push(&mut self, attr: StringifyAttribute) {
        self.attribute_stack.push(attr)
    }
}

impl<W: Write> DeclVisitor<'_> for StringifyVisitor<W> {
    fn visit_function_declaration(&mut self, fun: &FunctionDeclare) {
        self.write(format_args!("{} : ", TokenKind::Function));
        if let Some(ret_type) = fun.return_type() {
            self.write(format_args!("{} ", ret_type))
        }
        self.writeln(format_args!(""));

        // variable declarations
        let variables = fun.parameters();
        if !variables.is_empty() {
            let mut current_scope = None;
            for v in variables {
                // new group
                if current_scope != Some(v.scope()) {
                    if current_scope.is_some() {
                        self.writeln(format_args!("{}", TokenKind::EndVar));
                    }

                    self.writeln(format_args!("{}", variable_scope_start_tok(v.scope())));

                    current_scope = Some(v.scope());
                }

                // dump variable
                self.indent += 1;
                self.write_indent();
                self.writeln(format_args!(
                    "{}: {};",
                    v.name().origin_string(),
                    v.ty().expect("Variable type not exist!")
                ));
                self.indent -= 1;
            }

            // last group end
            if current_scope.is_some() {
                self.writeln(format_args!("{}", TokenKind::EndVar));
            }
        }

        self.writeln(format_args!("{}", TokenKind::EndFunction));
    }

    fn visit_enum_declaration(&mut self, decl: &'_ EnumDeclare) {
        self.writeln(format_args!(
            "{} {} {}",
            TokenKind::Type,
            decl.name().origin_string(),
            TokenKind::Colon
        ));
        self.writeln(format_args!("{}", TokenKind::LeftParentheses));

        // fields
        self.indent += 1;
        let field_count = decl.fields().len();
        for (index, field) in decl.fields().iter().enumerate() {
            self.write_indent();
            self.write(format_args!("{}", field.name().origin_string()));
            if let Some(val) = field.initial() {
                self.write(format_args!(" {} ", TokenKind::Assign));
                self.visit_expression(val);
            }

            if field_count == index + 1 {
                self.writeln(format_args!(""));
            } else {
                self.writeln(format_args!("{}", TokenKind::Comma))
            }
        }
        self.indent -= 1;

        // closed type, like: ) DINT;
        self.write(format_args!("{}", TokenKind::RightParentheses));
        if let Some(ty) = decl.ty() {
            self.write(format_args!(" {}", ty));
        }
        self.writeln(format_args!("{}", TokenKind::Semicolon));

        self.writeln(format_args!("{}", TokenKind::EndType))
    }
}

impl<W: Write> AstVisitor<'_> for StringifyVisitor<W> {
    fn visit_literal(&mut self, literal: &LiteralExpression) {
        match literal.literal() {
            LiteralValue::Bit(BitValue::Zero) => self.write(format_args!("{}", 0)),
            LiteralValue::Bit(BitValue::One) => self.write(format_args!("{}", 1)),
            LiteralValue::Bool(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::Int(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::UInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::Byte(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::SInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::DInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::UDInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::LInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::ULInt(x) => self.write(format_args!("{:?}", x)),
            LiteralValue::Real(x) => self.write(format_args!("{}", x)),
            LiteralValue::LReal(x) => self.write(format_args!("{}", x)),
            LiteralValue::String(x) => self.write(format_args!("{:?}", x)),
        }
    }

    #[inline]
    fn visit_variable_expression(&mut self, expr: &'_ ExprInfo, variable: &'_ VariableExpression) {
        self.write(format_args!("{}", variable.org_name()));
    }

    fn visit_call_expression(&mut self, call: &'_ CallExpression) {
        self.visit_expression(call.callee());

        self.write(format_args!("{}", TokenKind::LeftParentheses));
        let mut first = true;
        for arg in call.arguments() {
            if !first {
                self.write(format_args!("{} ", TokenKind::Comma));
            }

            self.visit_expression(arg);

            if first {
                first = false;
            }
        }

        self.write(format_args!("{}", TokenKind::RightParentheses));
    }

    fn visit_expr_statement(&mut self, _: &Statement, expr_st: &ExprStatement) {
        self.write_indent();
        self.visit_expression(expr_st.expr());
        self.writeln(format_args!(";"));
    }

    fn visit_if_statement(&mut self, _: &StmtInfo, stmt: &IfStatement) {
        self.write_indent();
        self.write(format_args!("IF "));
        self.visit_expression(stmt.condition());
        self.writeln(format_args!(" THEN"));

        if let Some(then_controlled) = stmt.then_controlled() {
            self.indent += 1;
            self.visit_statement(then_controlled);
            self.indent -= 1;
        }

        for elseif in stmt.else_if_list() {
            self.write(format_args!("ELSEIF "));
            self.visit_expression(elseif.condition());
            self.writeln(format_args!(" THEN"));

            if let Some(elseif_then) = elseif.then_controlled() {
                self.indent += 1;
                self.visit_statement(elseif_then);
                self.indent -= 1;
            }
        }

        if let Some(else_controlled) = stmt.else_controlled() {
            self.writeln(format_args!("ELSE"));

            self.indent += 1;
            self.visit_statement(else_controlled);
            self.indent -= 1;
        }

        self.writeln(format_args!("END_IF"));
    }

    fn visit_operator_expression(&mut self, expr: &OperatorExpression) {
        let sub_expression = self.top().map(|x| x.sub_expression).unwrap_or(false);

        if sub_expression {
            self.write(format_args!("("));
        }

        let op = *expr.op();
        let operands = expr.operands();

        if operands.len() == 1 {
            match op {
                Operator::Not => self.write(format_args!("{} ", TokenKind::Not)),
                Operator::Minus => self.write(format_args!("{}", TokenKind::Minus)),
                _ => panic!("invalid unary operator!"),
            };

            self.push(StringifyAttribute::sub_expression());
            self.visit_expression(&operands[0]);
            self.pop();
        } else {
            assert_eq!(operands.len(), 2);

            self.push(StringifyAttribute::sub_expression());
            self.visit_expression(&operands[0]);
            self.pop();

            self.write(format_args!(" {} ", op));

            self.push(StringifyAttribute::sub_expression());
            self.visit_expression(&operands[1]);
            self.pop();
        }

        if sub_expression {
            self.write(format_args!(")"));
        }
    }

    fn visit_assign_expression(&mut self, assign: &AssignExpression) {
        self.visit_expression(assign.left());
        self.write(format_args!(" {} ", assign.assign_type()));
        self.visit_expression(assign.right());
    }

    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression) {
        self.visit_expression(compo.left());
        self.write(format_args!("{}", TokenKind::DotAccess));
        self.visit_expression(compo.right());
    }
}

#[inline]
fn variable_scope_start_tok(class: VariableFlags) -> TokenKind {
    match class {
        VariableFlags::NONE => TokenKind::Var,
        VariableFlags::GLOBAL => TokenKind::VarGlobal,
        _ => unimplemented!(),
    }
}

#[cfg(test)]
mod test {
    use crate::ast::AstVisitor;
    use crate::parser::*;
    use crate::utils::*;

    fn parse_and_stringify<S: AsRef<str>>(s: S) -> String {
        let mut lexer = StLexerBuilder::new().build_str(s.as_ref());
        let r = ParserBuilder::default()
            .build()
            .parse_stmt(&mut lexer)
            .unwrap();

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        stringify.visit_statement(&r);

        String::from_utf8_lossy(&buf).into()
    }

    #[test]
    fn stringify() {
        let buf_str = parse_and_stringify("2-3.0/3; -1+\"a\\\"s\\\"d\";");
        assert_eq!(buf_str, "2 - (3.0 / 3);\n(-1) + \"a\\\"s\\\"d\";\n");

        let buf_str = parse_and_stringify("2-3.0/3; NOT 1+\"a\\\"s\\\"d\";");
        assert_eq!(buf_str, "2 - (3.0 / 3);\n(NOT 1) + \"a\\\"s\\\"d\";\n");
    }

    #[test]
    fn test_if_else() {
        let buf_str = parse_and_stringify("if a - 1 then a + 1; else a - 1; end_if");

        assert_eq!(
            buf_str,
            "IF a - 1 THEN\n    a + 1;\nELSE\n    a - 1;\nEND_IF\n"
        );
    }

    #[test]
    fn test_assign_expr() {
        let buf_str = parse_and_stringify("if a - 1 then a:= a + 1; else a - 1; end_if");

        assert_eq!(
            buf_str,
            "IF a - 1 THEN\n    a := a + 1;\nELSE\n    a - 1;\nEND_IF\n"
        );
    }

    #[test]
    fn test_sub_expr_parenthesis() {
        let buf_str = parse_and_stringify("a * (a + 1);");

        assert_eq!(buf_str, "a * (a + 1);\n");
    }

    #[test]
    fn test_call_expression() {
        let buf_str = parse_and_stringify("f(\"a, b\", c => d);");

        assert_eq!(buf_str, "f(\"a, b\", c => d);\n");
    }
}
