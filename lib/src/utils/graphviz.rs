use crate::ast::*;
use crate::parser::{Location, TokenKind};
use crate::utils::{AstHasher, Crc32Hasher, StringifyVisitor};
use chrono::Local;
use regex::Regex;
use smallvec::smallvec;
use std::fmt::Arguments;
use std::io::Write;
use std::iter::FromIterator;

struct EscapedString(String);

impl<S: AsRef<str>> From<S> for EscapedString {
    fn from(value: S) -> Self {
        let regex = Regex::new(r#"([<>|"])"#).unwrap();
        let value = regex.replace_all(value.as_ref(), "\\$1");

        Self(value.to_string())
    }
}

/// Labels in same line
struct Labels(SmallVec8<EscapedString>);

/// SubLabel connect to another node, The Label name is not escape
struct SubLabel(String, String);

impl SubLabel {
    pub fn new<S1: Into<String>, S2: Into<String>>(pos: S1, label: S2) -> Self {
        Self(pos.into(), label.into())
    }
}

impl From<SubLabel> for EscapedString {
    fn from(value: SubLabel) -> Self {
        // self.0 is position mark, only self.1 should be escaped
        let escaped = EscapedString::from(value.1);
        EscapedString(format!("<{}> {}", value.0, escaped.0))
    }
}

impl<S> FromIterator<S> for Labels
where
    S: AsRef<str>,
{
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        Self(iter.into_iter().map(|x| x.as_ref().into()).collect())
    }
}

impl FromIterator<SubLabel> for Labels {
    fn from_iter<T: IntoIterator<Item = SubLabel>>(iter: T) -> Self {
        Self(iter.into_iter().map(|x| x.into()).collect())
    }
}

impl From<Labels> for String {
    fn from(value: Labels) -> Self {
        // Extract raw String from Labels to join
        value
            .0
            .into_iter()
            .map(|x| x.0)
            .collect::<Vec<_>>()
            .join("|")
    }
}

impl<S: AsRef<str>> From<S> for Labels {
    fn from(value: S) -> Self {
        Self(smallvec![value.as_ref().into()])
    }
}

/// Label group
struct LabelGroups(SmallVec8<Labels>);

impl LabelGroups {
    pub fn new<S: AsRef<str>>(label: S) -> Self {
        Self(smallvec![label.into()])
    }

    pub fn append_label<S: AsRef<str>>(mut self, label: S) -> Self {
        if self.0.is_empty() {
            self.0.push(label.into());
            return self;
        }

        self.0.last_mut().unwrap().0.push(label.as_ref().into());
        self
    }

    pub fn append_group<T: Into<Labels>>(mut self, labels: T) -> Self {
        self.0.push(labels.into());
        self
    }

    pub fn append_label_opt<S: AsRef<str>>(self, label: Option<S>) -> Self {
        match label {
            Some(l) => self.append_label(l),
            None => self,
        }
    }
}

impl<S> FromIterator<S> for LabelGroups
where
    S: AsRef<str>,
{
    fn from_iter<T: IntoIterator<Item = S>>(iter: T) -> Self {
        let labels = Labels::from_iter(iter);

        Self(smallvec![labels])
    }
}

impl From<LabelGroups> for String {
    fn from(value: LabelGroups) -> Self {
        let groups = value
            .0
            .into_iter()
            .map(|x| format!("{{{}}}", Into::<String>::into(x)))
            .collect::<Vec<_>>();

        format!("{{{}}}", groups.join("|"))
    }
}

fn location_label(start: Option<Location>, end: Option<Location>) -> Option<String> {
    match (start, end) {
        (None, None) => None,
        (Some(loc), None) | (None, Some(loc)) => Some(format!("{},{}", loc.mark, loc.offset)),
        (Some(start), Some(end)) => Some(format!(
            "{},{}:{},{}",
            start.mark, start.offset, end.mark, end.offset
        )),
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
}

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
        self.epilogue();
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

    fn epilogue(&mut self) {
        self.writeln(format_args!("}}"));
    }

    fn unique_node<S: AsRef<str>>(&mut self, name: S) -> String {
        self.unique_name_id += 1;
        format!("{}_{}", name.as_ref(), self.unique_name_id)
    }

    fn sub_label_to_new_node<S: AsRef<str>>(&mut self, sub_label_name: S) -> (String, SubLabel) {
        self.unique_name_id += 1;

        let pos = format!("pos_{}_{}", sub_label_name.as_ref(), self.unique_name_id);
        let sub_label = SubLabel::new(&pos, sub_label_name.as_ref());

        (pos, sub_label)
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

    fn write_node<S: AsRef<str>>(&mut self, node: S, label_group: LabelGroups) {
        self.writeln(format_args!(
            "{} [label=\"{}\"]",
            node.as_ref(),
            Into::<String>::into(label_group)
        ));
    }

    fn connect_sub<S1: AsRef<str>, S2: AsRef<str>, S3: AsRef<str>>(
        &mut self,
        from: S1,
        from_sub: S2,
        to: S3,
    ) {
        self.writeln(format_args!(
            "{}:{} -> {};",
            from.as_ref(),
            from_sub.as_ref(),
            to.as_ref()
        ))
    }

    fn connect<S1: AsRef<str>, S3: AsRef<str>>(&mut self, from: S1, to: S3) {
        self.writeln(format_args!("{} -> {};", from.as_ref(), to.as_ref()))
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
        let name = self.unique_node("declaration_statement");

        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        stringify.visit_declaration(decl);

        let s = String::from_utf8_lossy(&buf);
        let info_groups = LabelGroups::new("Declaration").append_group::<Labels>(s.into());
        self.write_node(&name, info_groups);
    }
}

impl<W: Write> AstVisitor<'_> for GraphvizExporter<W> {
    fn visit_literal(&mut self, literal: &LiteralExpression) {
        let name = self.unique_node("literal");
        let labels = [
            format!("Literal: {}", literal.literal()),
            format!("Type: {}", literal.literal().ty().type_class()),
        ];
        self.write_node(&name, LabelGroups::from_iter(labels));

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_variable_expression(&mut self, info: &'_ ExprInfo, variable: &'_ VariableExpression) {
        let name = self.unique_node("variable");

        let loc_group = location_label(info.start, info.end);
        let basic_info = format!(
            "{}: {}",
            variable.name().origin_string(),
            display_type(variable.ty())
        );
        let info_group = LabelGroups::new(basic_info).append_label_opt(loc_group);
        self.write_node(&name, info_group);

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
        let node = self.unique_node("statement_list");
        let mut labels = Vec::with_capacity(stmts.len());
        for (i, s) in stmts.iter().enumerate() {
            let sub_node = self.unique_node("pos");
            labels.push(SubLabel::new(&sub_node, i.to_string()));

            self.push_empty();
            self.visit_statement(s);
            let attr = self.pop();

            self.connect_sub(&node, sub_node, attr.node_name);
        }

        let info_groups = LabelGroups::new("StmtList").append_group(Labels::from_iter(labels));
        self.write_node(&node, info_groups);

        if let Some(top) = self.top_mut() {
            top.node_name = node;
        }
    }

    fn visit_expr_statement(&mut self, info: &StmtInfo, expr_st: &ExprStatement) {
        let node = self.unique_node("expr_statement");

        self.push_empty();
        self.visit_expression(expr_st.expr());
        let attr = self.pop();

        let location = location_label(info.start_pos, info.end_pos);
        let info_group = LabelGroups::new("ExprStmt")
            .append_label_opt(location)
            .append_group::<Labels>(format!("{}{}", expr_st.expr(), TokenKind::Semicolon).into());
        self.write_node(&node, info_group);
        self.connect(&node, attr.node_name);
        if let Some(top) = self.top_mut() {
            top.node_name = node;
        }
    }

    fn visit_if_statement(&mut self, info: &StmtInfo, ifst: &IfStatement) {
        let name = self.unique_node("if_statement");

        // condition + then + else + else-if-list
        let mut labels = Vec::with_capacity(ifst.else_if_list().len() + 3);

        self.push_empty();
        self.visit_expression(ifst.condition());
        let attr = self.pop();

        let (pos, label) = self.sub_label_to_new_node("Cond");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        if let Some(then) = ifst.then_controlled() {
            self.push_empty();
            self.visit_statement(then);
            let attr = self.pop();

            let (pos, label) = self.sub_label_to_new_node("Then");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        // else if list
        for else_if in ifst.else_if_list() {
            let else_if_node = self.unique_node("else_if_statement");
            let mut else_if_labels = vec![];

            let (pos, label) = self.sub_label_to_new_node("ElseIf");
            self.connect_from_pos(&name, pos, &else_if_node);
            labels.push(label);

            self.push_empty();
            self.visit_expression(else_if.condition());
            let attr = self.pop();

            let (pos, label) = self.sub_label_to_new_node("ElseIfCond");
            self.connect_from_pos(&else_if_node, pos, attr.node_name);
            else_if_labels.push(label);

            if let Some(controlled) = else_if.then_controlled() {
                self.push_empty();
                self.visit_statement(controlled);
                let attr = self.pop();

                let (pos, label) = self.sub_label_to_new_node("ElseIfThen");
                self.connect_from_pos(&else_if_node, pos, attr.node_name);
                else_if_labels.push(label);
            }

            let else_if_info_groups =
                LabelGroups::new("ElseIfStmt").append_group(Labels::from_iter(else_if_labels));
            self.write_node(&else_if_node, else_if_info_groups);
        }

        if let Some(else_ctrl) = ifst.else_controlled() {
            self.push_empty();
            self.visit_statement(else_ctrl);
            let attr = self.pop();

            let (pos, label) = self.sub_label_to_new_node("Else");
            self.connect_from_pos(&name, pos, attr.node_name);
            labels.push(label);
        }

        let location = location_label(info.start_pos, info.end_pos);
        let info_groups = LabelGroups::new("IfStatement")
            .append_label_opt(location)
            .append_group(Labels::from_iter(labels));
        self.write_node(&name, info_groups);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_operator_expression(&mut self, expr: &OperatorExpression) {
        let name = self.unique_node("operator_expression");

        let labels = [
            format!("Operator '{}'", &expr.op()),
            format!("Type: {}", display_type(expr.ty())),
        ];

        let info_groups =
            LabelGroups::from_iter(labels).append_group::<Labels>(expr.to_string().into());
        self.write_node(&name, info_groups);

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
        let name = self.unique_node("assign_expression");

        let mut labels = Vec::with_capacity(2);

        self.push_empty();
        self.visit_expression(assign.left());
        let attr = self.pop();

        let (pos, label) = self.sub_label_to_new_node("Left");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        self.push_empty();
        self.visit_expression(assign.right());
        let attr = self.pop();

        let (pos, label) = self.sub_label_to_new_node("Right");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        let titles = [
            "AssignExpr",
            &format!("Type: {}", display_type(assign.ty())),
        ];

        let info_groups = LabelGroups::from_iter(titles)
            .append_group::<Labels>(assign.to_string().into())
            .append_group(Labels::from_iter(labels));
        self.write_node(&name, info_groups);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }

    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression) {
        let name = self.unique_node("compo_access_expression");

        let mut labels = vec![];

        self.push_empty();
        self.visit_expression(compo.left());
        let attr = self.pop();

        let (pos, label) = self.sub_label_to_new_node("Left");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        self.push_empty();
        self.visit_expression(compo.right());
        let attr = self.pop();

        let (pos, label) = self.sub_label_to_new_node("Right");
        self.connect_from_pos(&name, pos, attr.node_name);
        labels.push(label);

        let info_group = LabelGroups::new("CompoAccess").append_group(Labels::from_iter(labels));
        self.write_node(&name, info_group);

        if let Some(top) = self.top_mut() {
            top.node_name = name;
        }
    }
}
