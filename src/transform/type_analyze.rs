use crate::ast::*;
use crate::parser::LiteralType;

pub struct TypeAnalyzer;

impl TypeAnalyzer {
    pub fn new() -> Self {
        Self {}
    }

    pub fn analyze(&mut self, ast: &mut dyn AstNode) {
        ast.accept_mut(self)
    }
}

impl AstVisitorMut for TypeAnalyzer {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType) {
        todo!()
    }

    fn visit_variable_mut(&mut self, variable: &mut Variable) {
        todo!()
    }

    fn visit_statement_list_mut(&mut self, stmt: &mut StatementList) {
        for s in stmt.statements_mut() {
            s.accept_mut(self)
        }
    }

    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement) {
        stmt.expr_mut().accept_mut(self)
    }

    fn visit_if_statement_mut(&mut self, stmt: &mut IfStatement) {
        todo!()
    }

    fn visit_declaration_statement_mut(&mut self, decl: &mut DeclarationStatement) {
        todo!()
    }

    fn visit_operator_expression_mut(&mut self, expr: &mut OperatorExpression) {
        todo!()
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        todo!()
    }

    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression) {
        todo!()
    }
}
