use crate::ast::*;

// Immutable visitor
pub trait AstVisitor {
    fn visit_literal(&mut self, _: &LiteralType);
    fn visit_variable(&mut self, _: &Variable);
    fn visit_statement_list(&mut self, _: &StatementList);
    fn visit_expr_statement(&mut self, _: &ExprStatement);
    fn visit_if_statement(&mut self, _: &IfStatement);
    fn visit_declaration_statement(&mut self, _: &DeclarationStatement);
    fn visit_operator_expression(&mut self, _: &OperatorExpression);
    fn visit_assign_expression(&mut self, _: &AssignExpression);
    fn visit_compo_access_expression(&mut self, _: &CompoAccessExpression);
}

// Mutable visitor
pub trait AstVisitorMut {
    fn visit_literal_mut(&mut self, _: &mut LiteralType);
    fn visit_variable_mut(&mut self, _: &mut Variable);
    fn visit_statement_list_mut(&mut self, _: &mut StatementList);
    fn visit_expr_statement_mut(&mut self, _: &mut ExprStatement);
    fn visit_if_statement_mut(&mut self, _: &mut IfStatement);
    fn visit_declaration_statement_mut(&mut self, _: &mut DeclarationStatement);
    fn visit_operator_expression_mut(&mut self, _: &mut OperatorExpression);
    fn visit_assign_expression_mut(&mut self, _: &mut AssignExpression);
    fn visit_compo_access_expression_mut(&mut self, _: &mut CompoAccessExpression);
}

pub trait DeclarationVisitor {
    fn visit_function_declare(&mut self, _: &FunctionDeclaration);
}
