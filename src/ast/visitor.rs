use crate::ast::*;

// Immutable visitor
pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &LiteralType);
    fn visit_identifier(&mut self, identifier: &IdentifierExpression);
    fn visit_statement_list(&mut self, stmt: &StatementList);
    fn visit_expr_statement(&mut self, stmt: &ExprStatement);
    fn visit_if_statement(&mut self, stmt: &IfStatement);
    fn visit_declaration_statement(&mut self, decl: &DeclarationStatement);
    fn visit_operator_expression(&mut self, expr: &OperatorExpression);
    fn visit_assign_expression(&mut self, assign: &AssignExpression);
    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression);
    fn visit_function_declaration(&mut self, fun: &FunctionDeclaration);
}

// Mutable visitor
pub trait AstVisitorMut: AstVisitor {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType);
    fn visit_identifier_mut(&mut self, identifier: &mut IdentifierExpression);
    fn visit_statement_list_mut(&mut self, stmt: &mut StatementList);
    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement);
    fn visit_if_statement_mut(&mut self, stmt: &mut IfStatement);
    fn visit_declaration_statement_mut(&mut self, decl: &mut DeclarationStatement);
    fn visit_operator_expression_mut(&mut self, expr: &mut OperatorExpression);
    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression);
    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression);
    fn visit_function_declaration_mut(&mut self, fun: &mut FunctionDeclaration);
}
