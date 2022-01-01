use crate::ast::*;
use crate::LiteralValue;

pub struct BackendAdapter {}

impl AstVisitorMut for BackendAdapter {
    fn visit_literal_mut(&mut self, _: &mut LiteralValue) {
        todo!()
    }

    fn visit_variable_mut(&mut self, _: &mut Variable) {
        todo!()
    }

    fn visit_statement_list_mut(&mut self, _: &mut StatementList) {
        todo!()
    }

    fn visit_expr_statement_mut(&mut self, _: &mut ExprStatement) {
        todo!()
    }

    fn visit_if_statement_mut(&mut self, _: &mut IfStatement) {
        todo!()
    }

    fn visit_declaration_statement_mut(&mut self, _: &mut DeclarationStatement) {
        todo!()
    }

    fn visit_operator_expression_mut(&mut self, _: &mut OperatorExpression) {
        todo!()
    }

    fn visit_assign_expression_mut(&mut self, _: &mut AssignExpression) {
        todo!()
    }

    fn visit_compo_access_expression_mut(&mut self, _: &mut CompoAccessExpression) {
        todo!()
    }
}
