use super::RangeExpression;
use crate::ast::call_expression::CallExpression;
use crate::ast::*;

// Mutable visitor
pub trait DeclVisitorMut: Sized {
    #[inline]
    fn visit_declaration_mut(&mut self, decl: &mut Declaration) {
        walk_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_function_declaration_mut(&mut self, decl: &mut FunctionDeclare) {
        walk_function_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_enum_declaration_mut(&mut self, decl: &mut EnumDeclare) {
        walk_enum_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_alias_declaration_mut(&mut self, decl: &mut AliasDeclare) {
        walk_alias_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_struct_declaration_mut(&mut self, decl: &mut StructDeclare) {
        walk_struct_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_global_variable_declaration_mut(&mut self, decl: &mut GlobalVariableDeclare) {
        walk_global_variable_declaration_mut(self, decl)
    }

    #[inline]
    fn visit_variable_declaration_mut(&mut self, variable: &mut Variable) {
        walk_variable_declaration_mut(self, variable)
    }
}

pub trait AstVisitorMut: Sized {
    #[inline]
    fn visit_literal_mut(&mut self, literal: &mut LiteralExpression) {
        walk_literal_mut(self, literal)
    }

    #[inline]
    fn visit_variable_expression_mut(&mut self, variable: &mut Spanned<VariableExpression>) {
        walk_variable_expression_mut(self, variable)
    }

    #[inline]
    fn visit_call_expression_mut(&mut self, call: &mut CallExpression) {
        walk_call_expression_mut(self, call)
    }

    #[inline]
    fn visit_range_expression_mut(&mut self, range: &mut RangeExpression) {
        walk_range_expression_mut(self, range)
    }

    #[inline]
    fn visit_expression_mut(&mut self, expr: &mut Expression) {
        walk_expression_mut(self, expr)
    }

    #[inline]
    fn visit_statement_mut(&mut self, stmt: &mut Statement) {
        walk_statement_mut(self, stmt)
    }

    #[inline]
    fn visit_statement_list_mut(&mut self, stmts: &mut Vec<Statement>) {
        walk_statement_list_mut(self, stmts)
    }

    #[inline]
    fn visit_expr_statement_mut(&mut self, expr: &mut ExprStatement) {
        walk_expr_statement_mut(self, expr)
    }

    #[inline]
    fn visit_if_statement_mut(&mut self, info: &mut Option<LocSpan>, ifst: &mut IfStatement) {
        walk_if_statement_mut(self, info, ifst)
    }

    #[inline]
    fn visit_operator_expression_mut(&mut self, operator: &mut OperatorExpression) {
        walk_operator_expression_mut(self, operator)
    }

    #[inline]
    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        walk_assign_expression_mut(self, assign)
    }

    #[inline]
    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression) {
        walk_compo_access_expression_mut(self, compo)
    }
}

#[inline]
fn walk_literal_mut<V: AstVisitorMut>(_: &mut V, _: &mut LiteralExpression) {}

#[inline]
fn walk_variable_expression_mut<V: AstVisitorMut>(_: &mut V, _: &mut Spanned<VariableExpression>) {}

#[inline]
fn walk_call_expression_mut<V: AstVisitorMut>(_: &mut V, _: &mut CallExpression) {}

#[inline]
fn walk_range_expression_mut<V: AstVisitorMut>(_: &mut V, _: &mut RangeExpression) {}

#[inline]
fn walk_expression_mut<V: AstVisitorMut>(vis: &mut V, expr: &mut Expression) {
    match expr.kind {
        ExprKind::Assign(ref mut assign) => vis.visit_assign_expression_mut(assign),
        ExprKind::Operator(ref mut operator) => vis.visit_operator_expression_mut(operator),
        ExprKind::Compo(ref mut compo) => vis.visit_compo_access_expression_mut(compo),
        ExprKind::Variable(ref mut variable) => vis.visit_variable_expression_mut(variable),
        ExprKind::Literal(ref mut literal) => vis.visit_literal_mut(literal),
        ExprKind::Call(ref mut call) => vis.visit_call_expression_mut(call),
        ExprKind::Range(ref mut range) => vis.visit_range_expression_mut(range),
    }
}

#[inline]
fn walk_statement_mut<V: AstVisitorMut>(vis: &mut V, stmt: &mut Statement) {
    match stmt.kind {
        StmtKind::Expr(ref mut expr) => vis.visit_expr_statement_mut(expr),
        StmtKind::If(ref mut ifst) => vis.visit_if_statement_mut(&mut stmt.info, ifst),
        StmtKind::Stmts(ref mut v) => vis.visit_statement_list_mut(v),
    }
}

#[inline]
fn walk_statement_list_mut<V: AstVisitorMut>(vis: &mut V, stmts: &mut Vec<Statement>) {
    for stmt in stmts {
        vis.visit_statement_mut(stmt)
    }
}

#[inline]
fn walk_expr_statement_mut<V: AstVisitorMut>(vis: &mut V, expr: &mut ExprStatement) {
    vis.visit_expression_mut(expr.expr_mut())
}

#[inline]
fn walk_if_statement_mut<V: AstVisitorMut>(
    vis: &mut V,
    info: &mut Option<LocSpan>,
    ifst: &mut IfStatement,
) {
    vis.visit_expression_mut(ifst.condition_mut());
    if let Some(ctrl) = ifst.then_controlled_mut() {
        vis.visit_statement_mut(ctrl);
    }

    for elseif in ifst.else_if_list_mut() {
        vis.visit_expression_mut(elseif.condition_mut());
        if let Some(ctrl) = elseif.then_controlled_mut() {
            vis.visit_statement_mut(ctrl);
        }
    }

    if let Some(else_ctrl) = ifst.else_controlled_mut() {
        vis.visit_statement_mut(else_ctrl);
    }
}

#[inline]
fn walk_declaration_mut<V: DeclVisitorMut>(vis: &mut V, decl: &mut Declaration) {
    match decl.kind {
        DeclKind::Struct(ref mut struct_) => vis.visit_struct_declaration_mut(struct_),
        DeclKind::Enum(ref mut enum_) => vis.visit_enum_declaration_mut(enum_),
        DeclKind::Alias(ref mut alias) => vis.visit_alias_declaration_mut(alias),
        DeclKind::Fun(ref mut fun) => vis.visit_function_declaration_mut(fun),
        DeclKind::FB(ref mut fun) => vis.visit_function_declaration_mut(fun),
        DeclKind::Prg(ref mut fun) => vis.visit_function_declaration_mut(fun),
        DeclKind::GlobalVar(ref mut gv) => vis.visit_global_variable_declaration_mut(gv),
    }
}

#[inline]
fn walk_struct_declaration_mut<V: DeclVisitorMut>(_: &mut V, _: &mut StructDeclare) {}

#[inline]
fn walk_global_variable_declaration_mut<V: DeclVisitorMut>(
    _: &mut V,
    _: &mut GlobalVariableDeclare,
) {
}

#[inline]
fn walk_enum_declaration_mut<V: DeclVisitorMut>(_: &mut V, _: &mut EnumDeclare) {}

#[inline]
fn walk_alias_declaration_mut<V: DeclVisitorMut>(_: &mut V, _: &mut AliasDeclare) {}

#[inline]
fn walk_function_declaration_mut<V: DeclVisitorMut>(_: &mut V, _: &mut FunctionDeclare) {}

#[inline]
fn walk_variable_declaration_mut<V: DeclVisitorMut>(_: &mut V, _: &mut Variable) {}

#[inline]
fn walk_operator_expression_mut<V: AstVisitorMut>(vis: &mut V, operator: &mut OperatorExpression) {
    for operand in operator.operands_mut() {
        vis.visit_expression_mut(operand)
    }
}

#[inline]
fn walk_assign_expression_mut<V: AstVisitorMut>(vis: &mut V, assign: &mut AssignExpression) {
    vis.visit_expression_mut(assign.right_mut());
    vis.visit_expression_mut(assign.left_mut());
}

#[inline]
fn walk_compo_access_expression_mut<V: AstVisitorMut>(
    vis: &mut V,
    compo: &mut CompoAccessExpression,
) {
    vis.visit_expression_mut(compo.left_mut());
    vis.visit_expression_mut(compo.right_mut());
}

// Immutable visitor

pub trait DeclVisitor<'ast>: Sized {
    #[inline]
    fn visit_declaration(&mut self, decl: &'ast Declaration) {
        walk_declaration(self, decl)
    }

    #[inline]
    fn visit_function_declaration(&mut self, decl: &'ast FunctionDeclare) {
        walk_function_declaration(self, decl)
    }

    #[inline]
    fn visit_enum_declaration(&mut self, decl: &'ast EnumDeclare) {
        walk_enum_declaration(self, decl)
    }

    #[inline]
    fn visit_alias_declaration(&mut self, decl: &'ast AliasDeclare) {
        walk_alias_declaration(self, decl)
    }

    #[inline]
    fn visit_struct_declaration(&mut self, decl: &'ast StructDeclare) {
        walk_struct_declaration(self, decl)
    }

    #[inline]
    fn visit_global_variable_declaration(&mut self, decl: &'ast GlobalVariableDeclare) {
        walk_global_variable_declaration(self, decl)
    }

    #[inline]
    fn visit_variable_declaration(&mut self, variable: &'ast Variable) {
        walk_variable_declaration(self, variable)
    }
}

pub trait AstVisitor<'ast>: Sized {
    #[inline]
    fn visit_literal(&mut self, literal: &'ast LiteralExpression) {
        walk_literal(self, literal)
    }

    #[inline]
    fn visit_variable_expression(&mut self, variable: &'ast Spanned<VariableExpression>) {
        walk_variable_expression(self, variable)
    }

    #[inline]
    fn visit_call_expression(&mut self, call: &'ast CallExpression) {
        walk_call_expression(self, call)
    }

    #[inline]
    fn visit_range_expression(&mut self, range: &'ast RangeExpression) {
        walk_range_expression(self, range)
    }

    #[inline]
    fn visit_expression(&mut self, expr: &'ast Expression) {
        walk_expression(self, expr)
    }

    #[inline]
    fn visit_statement(&mut self, stmt: &'ast Statement) {
        walk_statement(self, stmt)
    }

    #[inline]
    fn visit_statement_list(&mut self, stmts: &'ast Vec<Statement>) {
        walk_statement_list(self, stmts)
    }

    #[inline]
    fn visit_expr_statement(&mut self, info: &'ast Option<LocSpan>, expr: &'ast ExprStatement) {
        walk_expr_statement(self, info, expr)
    }

    #[inline]
    fn visit_if_statement(&mut self, info: &'ast Option<LocSpan>, ifst: &'ast IfStatement) {
        walk_if_statement(self, info, ifst)
    }

    #[inline]
    fn visit_operator_expression(&mut self, operator: &'ast OperatorExpression) {
        walk_operator_expression(self, operator)
    }

    #[inline]
    fn visit_assign_expression(&mut self, assign: &'ast AssignExpression) {
        walk_assign_expression(self, assign)
    }

    #[inline]
    fn visit_compo_access_expression(&mut self, compo: &'ast CompoAccessExpression) {
        walk_compo_access_expression(self, compo)
    }
}

#[inline]
fn walk_literal<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a LiteralExpression) {}

#[inline]
fn walk_variable_expression<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a Spanned<VariableExpression>) {}

#[inline]
fn walk_call_expression<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a CallExpression) {}

#[inline]
fn walk_range_expression<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a RangeExpression) {}

#[inline]
fn walk_expression<'a, V: AstVisitor<'a>>(vis: &mut V, expr: &'a Expression) {
    match expr.kind {
        ExprKind::Assign(ref assign) => vis.visit_assign_expression(assign),
        ExprKind::Operator(ref operator) => vis.visit_operator_expression(operator),
        ExprKind::Compo(ref compo) => vis.visit_compo_access_expression(compo),
        ExprKind::Variable(ref variable) => vis.visit_variable_expression(variable),
        ExprKind::Literal(ref literal) => vis.visit_literal(literal),
        ExprKind::Call(ref call) => vis.visit_call_expression(call),
        ExprKind::Range(ref range) => vis.visit_range_expression(range),
    }
}

#[inline]
fn walk_statement<'a, V: AstVisitor<'a>>(vis: &mut V, stmt: &'a Statement) {
    match stmt.kind {
        StmtKind::Expr(ref expr) => vis.visit_expr_statement(&stmt.info, expr),
        StmtKind::If(ref ifst) => vis.visit_if_statement(&stmt.info, ifst),
        StmtKind::Stmts(ref v) => vis.visit_statement_list(v),
    }
}

#[inline]
fn walk_statement_list<'a, V: AstVisitor<'a>>(vis: &mut V, stmts: &'a Vec<Statement>) {
    for stmt in stmts {
        vis.visit_statement(stmt)
    }
}

#[inline]
fn walk_expr_statement<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    _: &'a Option<LocSpan>,
    expr: &'a ExprStatement,
) {
    vis.visit_expression(expr.expr())
}

fn walk_if_statement<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    _: &'a Option<LocSpan>,
    ifst: &'a IfStatement,
) {
    vis.visit_expression(ifst.condition());
    if let Some(ctrl) = ifst.then_controlled() {
        vis.visit_statement(ctrl);
    }

    for elseif in ifst.else_if_list() {
        vis.visit_expression(elseif.condition());
        if let Some(ctrl) = elseif.then_controlled() {
            vis.visit_statement(ctrl);
        }
    }

    if let Some(else_ctrl) = ifst.else_controlled() {
        vis.visit_statement(else_ctrl);
    }
}

#[inline]
fn walk_declaration<'a, V: DeclVisitor<'a>>(vis: &mut V, decl: &'a Declaration) {
    match decl.kind {
        DeclKind::Struct(ref struct_) => vis.visit_struct_declaration(struct_),
        DeclKind::Enum(ref enum_) => vis.visit_enum_declaration(enum_),
        DeclKind::Alias(ref alias) => vis.visit_alias_declaration(alias),
        DeclKind::Fun(ref fun) => vis.visit_function_declaration(fun),
        DeclKind::FB(ref fun) => vis.visit_function_declaration(fun),
        DeclKind::Prg(ref fun) => vis.visit_function_declaration(fun),
        DeclKind::GlobalVar(ref gv) => vis.visit_global_variable_declaration(gv),
    }
}

#[inline]
fn walk_struct_declaration<'a, V: DeclVisitor<'a>>(_: &mut V, _: &'a StructDeclare) {}

#[inline]
fn walk_enum_declaration<'a, V: DeclVisitor<'a>>(_: &mut V, _: &'a EnumDeclare) {}

#[inline]
fn walk_alias_declaration<'a, V: DeclVisitor<'a>>(_: &mut V, _: &'a AliasDeclare) {}

#[inline]
fn walk_function_declaration<'a, V: DeclVisitor<'a>>(_: &mut V, _: &'a FunctionDeclare) {}

#[inline]
fn walk_global_variable_declaration<'a, V: DeclVisitor<'a>>(
    _: &mut V,
    _: &'a GlobalVariableDeclare,
) {
}

#[inline]
fn walk_variable_declaration<'a, V: DeclVisitor<'a>>(_: &mut V, _: &'a Variable) {}

#[inline]
fn walk_operator_expression<'a, V: AstVisitor<'a>>(vis: &mut V, operator: &'a OperatorExpression) {
    for operand in operator.operands() {
        vis.visit_expression(operand);
    }
}

#[inline]
fn walk_assign_expression<'a, V: AstVisitor<'a>>(vis: &mut V, assign: &'a AssignExpression) {
    vis.visit_expression(assign.right());
    vis.visit_expression(assign.left());
}

#[inline]
fn walk_compo_access_expression<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    compo: &'a CompoAccessExpression,
) {
    vis.visit_expression(compo.left());
    vis.visit_expression(compo.right());
}
