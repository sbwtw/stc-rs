use crate::ast::*;
use crate::parser::{LiteralType, Tok};
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

    fn write_op(&mut self, op: &Tok) {
        write!(self.writer, "{}", op).unwrap();
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
            LiteralType::Real(x) => self.write(format_args!("{:?}", x)),
            LiteralType::Int(x) => self.write(format_args!("{:?}", x)),
            LiteralType::UInt(x) => self.write(format_args!("{:?}", x)),
            LiteralType::String(x) => self.write(format_args!("{:?}", x)),
        }
    }

    fn visit_variable(&mut self, variable: &Variable) {
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

        for elseif in stmt.else_if_list() {
            self.write(format_args!("ELSEIF "));
            elseif.condition().accept(self);
            self.writeln(format_args!(" THEN"));

            if let Some(elseif_then) = elseif.then_controlled() {
                self.indent += 1;
                elseif_then.accept(self);
                self.indent -= 1;
            }
        }

        if let Some(else_controlled) = stmt.else_controlled() {
            self.writeln(format_args!("ELSE"));

            self.indent += 1;
            else_controlled.accept(self);
            self.indent -= 1;
        }

        self.writeln(format_args!("END_IF"));
    }

    fn visit_declaration_statement(&mut self, decl: &DeclarationStatement) {
        decl.declaration().accept(self)
    }

    fn visit_operator_expression(&mut self, expr: &OperatorExpression) {
        let sub_expression = self.top().map(|x| x.sub_expression).unwrap_or(false);

        if sub_expression {
            self.write(format_args!("("));
        }

        let op = expr.op().clone();
        let operands = expr.operands();

        match op {
            Tok::Minus if operands.len() == 1 => {
                self.write_op(&op);

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
                self.write_op(&op);
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

    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression) {
        compo.left().accept(self);
        self.write(format_args!("."));
        compo.right().accept(self);
    }
}

fn variable_scope_start_tok(class: &VariableScopeClass) -> Tok {
    match class {
        VariableScopeClass::None => Tok::Var,
        VariableScopeClass::Global => Tok::VarGlobal,
        _ => unimplemented!(),
    }
}

impl<W: Write> DeclarationVisitor for StringifyVisitor<W> {
    fn visit_function_declare(&mut self, fun: &FunctionDeclaration) {
        self.write(format_args!("{} : ", Tok::Function));
        if let Some(ret_type) = fun.return_type() {
            self.write(format_args!("{} ", ret_type))
        }
        self.writeln(format_args!(""));

        // variable declarations
        let variables = fun.variables();
        if variables.len() > 0 {
            let mut current_scope = None;
            for v in variables {
                // new group
                if current_scope != Some(v.scope_class()) {
                    if current_scope.is_some() {
                        self.writeln(format_args!("{}", Tok::EndVar));
                    }

                    self.writeln(format_args!(
                        "{}",
                        variable_scope_start_tok(v.scope_class())
                    ));

                    current_scope = Some(v.scope_class());
                }

                // dump variable
                self.indent += 1;
                self.write_indent();
                v.accept(self);
                self.writeln(format_args!(
                    ": {};",
                    v.ty().expect("Variable type not exist!")
                ));
                self.indent -= 1;
            }

            // last group end
            if current_scope.is_some() {
                self.writeln(format_args!("{}", Tok::EndVar));
            }
        }

        self.writeln(format_args!("{}", Tok::EndFunction));
    }
}

#[cfg(test)]
mod test {
    use crate::parser::st::*;
    use crate::parser::*;
    use crate::utils::*;

    fn parse_string<S: AsRef<str>>(s: S) -> String {
        let lexer = Lexer::new(s.as_ref());
        let r = StFunctionParser::new().parse(lexer).unwrap();

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

        assert_eq!(
            buf_str,
            "IF a - 1 THEN\n    a + 1;\nELSE\n    a - 1;\nEND_IF\n"
        );
    }

    #[test]
    fn test_assign_expr() {
        let buf_str = parse_string("if a - 1 then a:= a + 1; else a - 1; end_if");

        assert_eq!(
            buf_str,
            "IF a - 1 THEN\n    a := a + 1;\nELSE\n    a - 1;\nEND_IF\n"
        );
    }

    #[test]
    fn test_sub_expr_parenthesis() {
        let buf_str = parse_string("a * (a + 1);");

        assert_eq!(buf_str, "a * (a + 1);\n");
    }
}
