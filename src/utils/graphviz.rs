use crate::ast::*;
use crate::parser::LiteralType;
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

    fn append_group(mut self, group: GraphvizLabelGroup) -> Self {
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

/// Labels group to string
impl Into<String> for GraphvizLabelGroup {
    fn into(self) -> String {
        match self {
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

    fn new<S: AsRef<str>>(name: S) -> Self {
        Self {
            node_name: name.as_ref().to_owned(),
        }
    }
}

pub struct GraphvizExporter<W: Write> {
    writer: W,
    unique_name_id: usize,
    attribute_stack: Vec<GraphvizAttribute>,
}

impl<W: Write> GraphvizExporter<W> {
    pub fn new(writer: W) -> Self {
        Self {
            writer,
            unique_name_id: 0,
            attribute_stack: vec![],
        }
    }

    pub fn plot(&mut self, ast: &dyn AstNode) {
        self.prolog();
        ast.accept(self);
        self.epilog();
    }

    fn prolog(&mut self) {
        self.writeln(format_args!("digraph ast {{"));
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

        let pos = format!("{}_{}", name.as_ref(), self.unique_name_id);
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

    fn connect_from_pos<S1: AsRef<str>, S2: AsRef<str>, S3: AsRef<str>>(&mut self, from: S1, from_pos: S2, to: S3) {
        self.writeln(format_args!("{}:{} -> {};", from.as_ref(), from_pos.as_ref(), to.as_ref()));
    }
}

/// LiteralType (type name, literal display)
fn literal_display(literal: &LiteralType) -> (String, String) {
    match literal {
        LiteralType::I32(x) => ("i32".to_owned(), format!("{:?}", x)),
        LiteralType::U64(x) => ("u64".to_owned(), format!("{:?}", x)),
        LiteralType::F32(x) => ("f32".to_owned(), format!("{:?}", x)),
        LiteralType::String(x) => ("string".to_owned(), x.to_owned()),
    }
}

fn opcode_display(op: &OpCode) -> String {
    match op {
        OpCode::Mul => String::from("*"),
        OpCode::Div => String::from("/"),
        OpCode::Add => String::from("+"),
        OpCode::Sub => String::from("-"),
    }
}

impl<W: Write> AstVisitor for GraphvizExporter<W> {
    fn visit_literal(&mut self, literal: &LiteralType) {
        let name = self.unique_name("literal");
        let (ty, display) = literal_display(literal);
        let labels = [format!("Literal: {}", display), format!("Type: {}", ty)];
        self.write_node(&name, GraphvizLabelGroup::from_iter(&labels));

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_variable(&mut self, variable: &VariableExpression) {
        let name = self.unique_name("variable");
        self.write_node(
            &name,
            format!("Variable: {}", variable.origin_name()).into(),
        );

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_statement_list(&mut self, stmt: &StatementList) {
        let name = self.unique_name("statement_list");
        let mut labels = vec![];
        for (i, s) in stmt.0.iter().enumerate() {
            let pos = self.unique_name("pos");
            labels.push(format!("<{}> {}", &pos, i));

            self.push_empty();
            s.accept(self);
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

    fn visit_expr_statement(&mut self, stmt: &ExprStatement) {
        let name = self.unique_name("expr_statement");

        self.push_empty();
        stmt.expr().accept(self);
        let attr = self.pop();

        self.write_node(&name, GraphvizLabelGroup::from_name("ExprStatement"));
        self.connect(&name, attr.node_name);
        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_if_statement(&mut self, stmt: &IfStatement) {
        let name = self.unique_name("if_statement");

        let mut labels = vec![];

        self.push_empty();
        stmt.condition().accept(self);
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Cond");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        if let Some(then) = stmt.then_controlled() {
            self.push_empty();
            then.accept(self);
            let attr = self.pop();

            let (pos, label) = self.unique_name_with_pos("Then");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        if let Some(else_ctrl) = stmt.else_controlled() {
            self.push_empty();
            else_ctrl.accept(self);
            let attr = self.pop();

            let (pos, label) = self.unique_name_with_pos("Else");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        let labels = GraphvizLabelGroup::from_name("IfStatement")
            .append_group(GraphvizLabelGroup::from_iter(&labels));
        self.write_node(&name, labels);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_operator_expression(&mut self, op: &OpCode, operands: &[Box<dyn Expression>]) {
        let name = self.unique_name("operator_expression");

        let labels = format!("Operator '{}'", opcode_display(op)).into();
        self.write_node(&name, labels);

        for operand in operands {
            self.push_empty();
            operand.accept(self);
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
        assign.left().accept(self);
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Left");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        self.push_empty();
        assign.right().accept(self);
        let attr = self.pop();

        let (pos, label) = self.unique_name_with_pos("Right");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        let labels = GraphvizLabelGroup::from_name("AssignExpr")
            .append_group(GraphvizLabelGroup::from_iter(&labels));
        self.write_node(&name, labels);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }
}
