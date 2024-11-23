use crate::ast::*;
use crate::parser::Location;
use crate::utils::{AstHasher, Crc32Hasher, StringifyVisitor};
use chrono::Local;
use regex::Regex;
use std::borrow::Cow;
use std::fmt::Arguments;
use std::io::Write;
use std::iter::FromIterator;

/// Graphviz Labels Group
enum GraphvizLabelGroup {
    Labels(Vec<String>),
    Groups(Vec<GraphvizLabelGroup>),
}

impl GraphvizLabelGroup {
    fn from_name<S: AsRef<str>>(name: S) -> Self {
        GraphvizLabelGroup::Labels(vec![name.as_ref().to_owned()])
    }

    #[inline]
    fn from_name_with_location<S: AsRef<str>>(
        name: S,
        start: Option<Location>,
        end: Option<Location>,
    ) -> Self {
        let loc = match (start, end) {
            (None, None) => return Self::from_name(name),
            (Some(loc), None) | (None, Some(loc)) => {
                format!("{},{}", loc.mark, loc.offset)
            }
            (Some(start), Some(end)) => {
                format!(
                    "{},{}:{},{}",
                    start.mark, start.offset, end.mark, end.offset
                )
            }
        };

        GraphvizLabelGroup::Labels(vec![name.as_ref().to_owned(), loc])
    }

    fn append_group(self, group: GraphvizLabelGroup) -> Self {
        match self {
            GraphvizLabelGroup::Labels(labels) => {
                let g = GraphvizLabelGroup::Labels(labels);
                GraphvizLabelGroup::Groups(vec![g, group])
            }
            GraphvizLabelGroup::Groups(mut groups) => {
                groups.push(group);
                GraphvizLabelGroup::Groups(groups)
            }
        }
    }

    #[inline]
    fn append_group_opt(self, opt_group: Option<GraphvizLabelGroup>) -> Self {
        match opt_group {
            Some(g) => self.append_group(g),
            None => self,
        }
    }
}

/// Single string to Labels
impl<S: AsRef<str>> From<S> for GraphvizLabelGroup {
    fn from(s: S) -> Self {
        GraphvizLabelGroup::Labels(vec![s.as_ref().to_owned()])
    }
}

/// A Iterator of string to labels
impl<S> FromIterator<S> for GraphvizLabelGroup
where
    S: AsRef<str>,
{
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        let mut labels = vec![];
        for i in iter {
            labels.push(i.as_ref().to_owned());
        }

        Self::Labels(labels)
    }
}

fn graphviz_escape<S: AsRef<str>>(s: &S) -> Cow<str> {
    let regex = Regex::new(r#"([<>|"])"#).unwrap();

    regex.replace_all(s.as_ref(), "\\$1")
}

/// Labels group to string
impl From<GraphvizLabelGroup> for String {
    fn from(value: GraphvizLabelGroup) -> Self {
        match value {
            GraphvizLabelGroup::Labels(labels) => labels.join(" | "),
            // sub group
            GraphvizLabelGroup::Groups(group) => {
                let mut groups = vec![];
                for g in group {
                    groups.push(format!("{{{}}}", Into::<String>::into(g)));
                }

                format!("{{{}}}", groups.join(" | "))
            }
        }
    }
}

struct GraphvizAttribute {
    node_name: String,
}

impl GraphvizAttribute {
    fn empty() -> Self {
        Self {
            node_name: String::new(),
        }
    }

    // fn new<S: AsRef<str>>(name: S) -> Self {
    //     Self {
    //         node_name: name.as_ref().to_owned(),
    //     }
    // }
}

#[allow(unused)]
pub struct GraphvizExporter<W: Write> {
    writer: W,
    unique_name_id: usize,
    attribute_stack: Vec<GraphvizAttribute>,
}

impl<W: Write> GraphvizExporter<W> {
    #[allow(unused)]
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            unique_name_id: 0,
            attribute_stack: vec![],
        }
    }

    // #[allow(unused)]
    // pub fn plot(&mut self, ast: &dyn AstNode) {
    //     self.prolog(ast);
    //     ast.accept(self);
    //     self.epilog();
    // }

    pub fn plot_statement(&mut self, stmt: &Statement) {
        self.prolog_statement(stmt);
        self.visit_statement(stmt);
        self.epilog();
    }

    fn prolog_statement(&mut self, stmt: &Statement) {
        let mut hasher = AstHasher::new(Crc32Hasher::new());
        let crc32 = hasher.calc_statement(stmt);

        self.writeln(format_args!("digraph ast {{"));
        self.writeln(format_args!(
            "label=\"time: {}, crc32: 0x{:X}\"",
            Local::now().format("%F %T"),
            crc32
        ));
        self.writeln(format_args!("node [shape=record style=rounded]"));
    }

    fn epilog(&mut self) {
        self.writeln(format_args!("}}"));
    }

    fn unique_name<S: AsRef<str>>(&mut self, name: S) -> String {
        self.unique_name_id += 1;
        format!("{}_{}", name.as_ref(), self.unique_name_id)
    }

    fn unique_name_with_pos<S: AsRef<str>>(&mut self, name: S) -> (String, String) {
        self.unique_name_id += 1;

        let pos = format!("P{}_{}", name.as_ref(), self.unique_name_id);
        let label = format!("<{}> {}", &pos, name.as_ref());

        (pos, label)
    }

    // fn top(&self) -> &GraphvizAttribute {
    //     self.attribute_stack.last().unwrap()
    // }

    fn top_mut(&mut self) -> Option<&mut GraphvizAttribute> {
        self.attribute_stack.last_mut()
    }

    fn pop(&mut self) -> GraphvizAttribute {
        self.attribute_stack.pop().unwrap()
    }

    fn push(&mut self, attr: GraphvizAttribute) {
        self.attribute_stack.push(attr)
    }

    fn push_empty(&mut self) {
        self.push(GraphvizAttribute::empty())
    }

    // fn write(&mut self, args: Arguments<'_>) {
    //     write!(self.writer, "{}", args).unwrap();
    // }

    fn writeln(&mut self, args: Arguments<'_>) {
        writeln!(self.writer, "{}", args).unwrap();
    }

    fn write_node<S: AsRef<str>>(&mut self, name: S, labels: GraphvizLabelGroup) {
        self.writeln(format_args!(
            "{} [label=\"{}\"]",
            name.as_ref(),
            Into::<String>::into(labels)
        ));
    }

    fn connect<S1: AsRef<str>, S2: AsRef<str>>(&mut self, from: S1, to: S2) {
        self.writeln(format_args!("{} -> {};", from.as_ref(), to.as_ref()));
    }

    fn connect_from_pos<S1: AsRef<str>, S2: AsRef<str>, S3: AsRef<str>>(
        &mut self,
        from: S1,
        from_pos: S2,
        to: S3,
    ) {
        self.writeln(format_args!(
            "{}:{} -> {};",
            from.as_ref(),
            from_pos.as_ref(),
            to.as_ref()
        ));
    }
}

fn display_type(ty: Option<&Type>) -> String {
    ty.map(|x| x.to_string()).unwrap_or_default()
}

impl<W: Write> DeclVisitor<'_> for GraphvizExporter<W> {
    fn visit_declaration(&mut self, decl: &Declaration) {
        let name = self.unique_name("declaration_statement");

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        stringify.visit_declaration(decl);

        let s = String::from_utf8_lossy(&buf);
        let labels = GraphvizLabelGroup::from_name("Declaration")
            .append_group(GraphvizLabelGroup::from_name(s));
        self.write_node(&name, labels);
    }
}

impl<W: Write> AstVisitor<'_> for GraphvizExporter<W> {
    fn visit_literal(&mut self, literal: &LiteralExpression) {
        let name = self.unique_name("literal");
        let labels = [
            format!("Literal: {}", literal.literal()),
            format!("Type: {}", literal.literal().ty().type_class()),
        ];
        self.write_node(&name, GraphvizLabelGroup::from_iter(&labels));

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_variable_expression(&mut self, variable: &'_ VariableExpression) {
        let name = self.unique_name("variable");

        let labels = [
            format!("Variable: {}", variable.name().origin_string()),
            format!("Type: {}", display_type(variable.ty())),
        ];
        self.write_node(&name, GraphvizLabelGroup::from_iter(&labels));

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_call_expression(&mut self, call: &'_ CallExpression) {
        let callee = call.callee().to_string();

        if let Some(top) = self.top_mut() {
            top.node_name = callee;
        }
    }

    fn visit_statement_list(&mut self, stmts: &Vec<Statement>) {
        let name = self.unique_name("statement_list");
        let mut labels = vec![];
        for (i, s) in stmts.iter().enumerate() {
            let pos = self.unique_name("pos");
            labels.push(format!("<{}> {}", &pos, i));

            self.push_empty();
            self.visit_statement(s);
            let attr = self.pop();

            self.connect(format!("{}:{}", &name, pos), attr.node_name);
        }

        let labels = GraphvizLabelGroup::from_name("StatementList")
            .append_group(GraphvizLabelGroup::from_iter(&labels));
        self.write_node(&name, labels);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_expr_statement(&mut self, stmt: &Statement, expr_st: &ExprStatement) {
        let name = self.unique_name("expr_statement");

        self.push_empty();
        self.visit_expression(expr_st.expr());
        let attr = self.pop();

        self.write_node(
            &name,
            GraphvizLabelGroup::from_name_with_location(
                "ExprStatement",
                stmt.start_pos,
                stmt.end_pos,
            )
            .append_group(GraphvizLabelGroup::from_name(graphviz_escape(
                &expr_st.expr().to_string(),
            ))),
        );
        self.connect(&name, attr.node_name);
        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_if_statement(&mut self, stmt: &Statement, ifst: &IfStatement) {
        let name = self.unique_name("if_statement");

        let mut labels = vec![];

        self.push_empty();
        self.visit_expression(ifst.condition());
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Cond");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        if let Some(then) = ifst.then_controlled() {
            self.push_empty();
            self.visit_statement(then);
            let attr = self.pop();

            let (pos, label) = self.unique_name_with_pos("Then");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        // else if list
        for else_if in ifst.else_if_list() {
            let else_if_node = self.unique_name("else_if_statement");
            let mut else_if_labels = vec![];

            let (pos, label) = self.unique_name_with_pos("ElseIf");
            self.connect_from_pos(&name, pos, &else_if_node);
            labels.push(label);

            self.push_empty();
            self.visit_expression(else_if.condition());
            let attr = self.pop();

            let (pos, label) = self.unique_name_with_pos("ElseIfCond");
            self.connect_from_pos(&else_if_node, pos, attr.node_name);
            else_if_labels.push(label);

            if let Some(controlled) = else_if.then_controlled() {
                self.push_empty();
                self.visit_statement(controlled);
                let attr = self.pop();

                let (pos, label) = self.unique_name_with_pos("ElseIfThen");
                self.connect_from_pos(&else_if_node, pos, attr.node_name);
                else_if_labels.push(label);
            }

            let else_if_labels = GraphvizLabelGroup::from_name("ElseIfStatement")
                .append_group(GraphvizLabelGroup::from_iter(else_if_labels));
            self.write_node(&else_if_node, else_if_labels);
        }

        if let Some(else_ctrl) = ifst.else_controlled() {
            self.push_empty();
            self.visit_statement(else_ctrl);
            let attr = self.pop();

            let (pos, label) = self.unique_name_with_pos("Else");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        let groups = GraphvizLabelGroup::from_name_with_location(
            "IfStatement",
            stmt.start_pos,
            stmt.end_pos,
        )
        .append_group(GraphvizLabelGroup::from_iter(&labels));

        self.write_node(&name, groups);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_operator_expression(&mut self, expr: &OperatorExpression) {
        let name = self.unique_name("operator_expression");

        let labels = [
            format!("Operator '{}'", graphviz_escape(&expr.op().to_string())),
            format!("Type: {}", display_type(expr.ty())),
        ];
        let labels = GraphvizLabelGroup::from_iter(&labels).append_group(
            GraphvizLabelGroup::from_name(graphviz_escape(&expr.to_string())),
        );
        self.write_node(&name, labels);

        for operand in expr.operands() {
            self.push_empty();
            self.visit_expression(operand);
            let attr = self.pop();

            self.connect(&name, attr.node_name);
        }

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_assign_expression(&mut self, assign: &AssignExpression) {
        let name = self.unique_name("assign_expression");

        let mut labels = vec![];

        self.push_empty();
        self.visit_expression(assign.left());
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Left");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        self.push_empty();
        self.visit_expression(assign.right());
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Right");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        let titles = [
            "AssignExpr",
            &format!("Type: {}", display_type(assign.ty())),
        ];

        let lines = GraphvizLabelGroup::from_iter(&titles)
            .append_group(GraphvizLabelGroup::from(assign.to_string()))
            .append_group(GraphvizLabelGroup::from_iter(&labels));
        self.write_node(&name, lines);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression) {
        let name = self.unique_name("compo_access_expression");

        let mut labels = vec![];

        self.push_empty();
        self.visit_expression(compo.left());
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Left");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        self.push_empty();
        self.visit_expression(compo.right());
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Right");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        let labels = GraphvizLabelGroup::from_name("CompoAccess")
            .append_group(GraphvizLabelGroup::from_iter(&labels));
        self.write_node(&name, labels);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }
}
