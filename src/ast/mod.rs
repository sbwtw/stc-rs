use std::fmt::{self, Debug, Display, Formatter};

use crate::parser::LiteralType;
use crate::utils::StringifyVisitor;

mod expr_statement;
pub use expr_statement::ExprStatement;

mod if_statement;
pub use if_statement::{ElseIfStatement, IfStatement};

mod literal_expression;
pub use literal_expression::LiteralExpression;

mod operator_expression;
pub use operator_expression::OperatorExpression;

mod variable_expression;
pub use variable_expression::VariableExpression;

mod assign_expression;
pub use assign_expression::AssignExpression;

mod compo_access_expression;
pub use compo_access_expression::CompoAccessExpression;

// Immutable visitor
pub trait AstVisitor {
    fn visit_literal(&mut self, literal: &LiteralType);
    fn visit_variable(&mut self, variable: &VariableExpression);
    fn visit_statement_list(&mut self, stmt: &StatementList);
    fn visit_expr_statement(&mut self, stmt: &ExprStatement);
    fn visit_if_statement(&mut self, stmt: &IfStatement);
    fn visit_operator_expression(&mut self, expr: &OperatorExpression);
    fn visit_assign_expression(&mut self, assign: &AssignExpression);
    fn visit_compo_access_expression(&mut self, compo: &CompoAccessExpression);
}

// Mutable visitor
pub trait AstVisitorMut: AstVisitor {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType);
    fn visit_variable_mut(&mut self, variable: &mut VariableExpression);
    fn visit_statement_list_mut(&mut self, stmt: &mut StatementList);
    fn visit_expr_statement_mut(&mut self, stmt: &mut ExprStatement);
    fn visit_if_statement_mut(&mut self, stmt: &mut IfStatement);
    fn visit_operator_expression_mut(&mut self, expr: &mut OperatorExpression);
    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression);
    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression);
}

pub trait AsAstNode {
    fn as_ast_node(&self) -> &dyn AstNode;
}

impl<T: AstNode> AsAstNode for T {
    fn as_ast_node(&self) -> &dyn AstNode {
        self
    }
}

pub trait AstNode: Debug + AsAstNode {
    fn accept(&self, visitor: &mut dyn AstVisitor);
    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut);
}

impl Display for &dyn AstNode {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut buf = vec![];
        let mut stringify = StringifyVisitor::new(&mut buf);
        self.accept(&mut stringify);

        write!(f, "{}", String::from_utf8_lossy(&buf))
    }
}

impl<T> AstNode for Box<T>
where
    T: AstNode,
{
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        self.as_ref().accept(visitor);
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        self.as_mut().accept_mut(visitor);
    }
}

pub trait Statement: AstNode {}

pub trait Expression: AstNode {}

#[derive(Debug)]
pub struct StatementList(pub Vec<Box<dyn Statement>>);

impl AstNode for StatementList {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_statement_list(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_statement_list(self)
    }
}

impl Statement for StatementList {}
