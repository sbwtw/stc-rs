use crate::ast::*;
use crate::parser::LiteralValue;

// Mutable visitor
pub trait AstVisitorMut<'ast>: Sized {
    fn visit_literal_mut(&mut self, literal: &'ast mut LiteralExpression) {
        walk_literal_mut(self, literal)
    }

    fn visit_variable_expression_mut(&mut self, variable: &'ast mut VariableExpression) {
        walk_variable_expression_mut(self, variable)
    }

    fn visit_expression_mut(&mut self, expr: &'ast mut Expression) {
        walk_expression_mut(self, expr)
    }

    fn visit_statement_mut(&mut self, stmt: &'ast mut Statement) {
        walk_statement_mut(self, stmt)
    }

    fn visit_statement_list_mut(&mut self, stmts: &'ast mut Vec<Statement>) {
        walk_statement_list_mut(self, stmts)
    }

    fn visit_expr_statement_mut(&mut self, expr: &'ast mut ExprStatement) {
        walk_expr_statement_mut(self, expr)
    }

    fn visit_if_statement_mut(&mut self, ifst: &'ast mut IfStatement) {
        walk_if_statement_mut(self, ifst)
    }

    fn visit_declaration_statement_mut(&mut self, decl: &'ast mut DeclarationStatement) {
        walk_declaration_statement_mut(self, decl)
    }

    fn visit_operator_expression_mut(&mut self, operator: &'ast mut OperatorExpression) {
        walk_operator_expression_mut(self, operator)
    }

    fn visit_assign_expression_mut(&mut self, assign: &'ast mut AssignExpression) {
        walk_assign_expression_mut(self, assign)
    }

    fn visit_compo_access_expression_mut(&mut self, compo: &'ast mut CompoAccessExpression) {
        walk_compo_access_expression_mut(self, compo)
    }
}

pub fn walk_literal_mut<'a, V: AstVisitorMut<'a>>(_: &mut V, _: &'a mut LiteralExpression) {}

pub fn walk_variable_expression_mut<'a, V: AstVisitorMut<'a>>(
    _: &mut V,
    _: &'a mut VariableExpression,
) {
}

pub fn walk_expression_mut<'a, V: AstVisitorMut<'a>>(vis: &mut V, expr: &'a mut Expression) {
    match expr.kind {
        ExprKind::Assign(ref mut assign) => vis.visit_assign_expression_mut(assign),
        ExprKind::Operator(ref mut operator) => vis.visit_operator_expression_mut(operator),
        ExprKind::Compo(ref mut compo) => vis.visit_compo_access_expression_mut(compo),
        ExprKind::Variable(ref mut variable) => vis.visit_variable_expression_mut(variable),
        ExprKind::Literal(ref mut literal) => vis.visit_literal_mut(literal),
    }
}

pub fn walk_statement_mut<'a, V: AstVisitorMut<'a>>(vis: &mut V, stmt: &'a mut Statement) {
    match stmt.kind {
        StmtKind::Decl(ref mut decl) => vis.visit_declaration_statement_mut(decl),
        StmtKind::Expr(ref mut expr) => vis.visit_expression_mut(expr.expr_mut()),
        StmtKind::If(ref mut ifst) => walk_if_statement_mut(vis, ifst),
        StmtKind::Stmts(ref mut v) => walk_statement_list_mut(vis, v),
    }
}

pub fn walk_statement_list_mut<'a, V: AstVisitorMut<'a>>(
    vis: &mut V,
    stmts: &'a mut Vec<Statement>,
) {
    for stmt in stmts {
        vis.visit_statement_mut(stmt)
    }
}

pub fn walk_expr_statement_mut<'a, V: AstVisitorMut<'a>>(vis: &mut V, expr: &'a mut ExprStatement) {
    vis.visit_expression_mut(expr.expr_mut())
}

pub fn walk_if_statement_mut<'a, V: AstVisitorMut<'a>>(vis: &mut V, ifst: &'a mut IfStatement) {
    // vis.visit_expression(ifst.condition());
    // if let Some(ctrl) = ifst.then_controlled() {
    //     vis.visit_statement(ctrl);
    // }
    //
    // for elseif in ifst.else_if_list() {
    //     vis.visit_expression(elseif.condition());
    //     if let Some(ctrl) = elseif.then_controlled() {
    //         vis.visit_statement(ctrl);
    //     }
    // }
    //
    // if let Some(else_ctrl) = ifst.else_controlled() {
    //     vis.visit_statement(else_ctrl);
    // }
}

pub fn walk_declaration_statement_mut<'a, V: AstVisitorMut<'a>>(
    vis: &mut V,
    decl: &'a mut DeclarationStatement,
) {
    todo!()
}

pub fn walk_operator_expression_mut<'a, V: AstVisitorMut<'a>>(
    vis: &mut V,
    operator: &'a mut OperatorExpression,
) {
    todo!()
}

pub fn walk_assign_expression_mut<'a, V: AstVisitorMut<'a>>(
    vis: &mut V,
    assign: &'a mut AssignExpression,
) {
    todo!()
}

pub fn walk_compo_access_expression_mut<'a, V: AstVisitorMut<'a>>(
    vis: &mut V,
    compo: &'a mut CompoAccessExpression,
) {
    todo!()
}

// Immutable visitor
pub trait AstVisitor<'ast>: Sized {
    fn visit_literal(&mut self, literal: &'ast LiteralExpression) {
        walk_literal(self, literal)
    }

    fn visit_variable_expression(&mut self, variable: &'ast VariableExpression) {
        walk_variable_expression(self, variable)
    }

    fn visit_expression(&mut self, expr: &'ast Expression) {
        walk_expression(self, expr)
    }

    fn visit_statement(&mut self, stmt: &'ast Statement) {
        walk_statement(self, stmt)
    }

    fn visit_statement_list(&mut self, stmts: &'ast Vec<Statement>) {
        walk_statement_list(self, stmts)
    }

    fn visit_expr_statement(&mut self, expr: &'ast ExprStatement) {
        walk_expr_statement(self, expr)
    }

    fn visit_if_statement(&mut self, ifst: &'ast IfStatement) {
        walk_if_statement(self, ifst)
    }

    fn visit_declaration_statement(&mut self, decl: &'ast DeclarationStatement) {
        walk_declaration_statement(self, decl)
    }

    fn visit_operator_expression(&mut self, operator: &'ast OperatorExpression) {
        walk_operator_expression(self, operator)
    }

    fn visit_assign_expression(&mut self, assign: &'ast AssignExpression) {
        walk_assign_expression(self, assign)
    }

    fn visit_compo_access_expression(&mut self, compo: &'ast CompoAccessExpression) {
        walk_compo_access_expression(self, compo)
    }
}

pub fn walk_literal<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a LiteralExpression) {}

pub fn walk_variable_expression<'a, V: AstVisitor<'a>>(_: &mut V, _: &'a VariableExpression) {}

pub fn walk_expression<'a, V: AstVisitor<'a>>(vis: &mut V, expr: &'a Expression) {
    match expr.kind {
        ExprKind::Assign(ref assign) => vis.visit_assign_expression(assign),
        ExprKind::Operator(ref operator) => vis.visit_operator_expression(operator),
        ExprKind::Compo(ref compo) => vis.visit_compo_access_expression(compo),
        ExprKind::Variable(ref variable) => vis.visit_variable_expression(variable),
        ExprKind::Literal(ref literal) => vis.visit_literal(literal),
    }
}

pub fn walk_statement<'a, V: AstVisitor<'a>>(vis: &mut V, stmt: &'a Statement) {
    match stmt.kind {
        StmtKind::Decl(ref decl) => vis.visit_declaration_statement(decl),
        StmtKind::Expr(ref expr) => vis.visit_expression(expr.expr()),
        StmtKind::If(ref ifst) => walk_if_statement(vis, ifst),
        StmtKind::Stmts(ref v) => walk_statement_list(vis, v),
    }
}

pub fn walk_statement_list<'a, V: AstVisitor<'a>>(vis: &mut V, stmts: &'a Vec<Statement>) {
    for stmt in stmts {
        vis.visit_statement(stmt)
    }
}

pub fn walk_expr_statement<'a, V: AstVisitor<'a>>(vis: &mut V, expr: &'a ExprStatement) {
    vis.visit_expression(expr.expr())
}

pub fn walk_if_statement<'a, V: AstVisitor<'a>>(vis: &mut V, ifst: &'a IfStatement) {
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

pub fn walk_declaration_statement<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    decl: &'a DeclarationStatement,
) {
    todo!()
}

pub fn walk_operator_expression<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    operator: &'a OperatorExpression,
) {
    todo!()
}

pub fn walk_assign_expression<'a, V: AstVisitor<'a>>(vis: &mut V, assign: &'a AssignExpression) {
    todo!()
}

pub fn walk_compo_access_expression<'a, V: AstVisitor<'a>>(
    vis: &mut V,
    compo: &'a CompoAccessExpression,
) {
    todo!()
}
