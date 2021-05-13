use crate::ast::*;
use crate::context::Scope;
use crate::parser::LiteralType;

struct TypeAnalyzerAttribute {
    scope: Scope,
    derived_type: Option<Box<dyn Type>>,
}

impl TypeAnalyzerAttribute {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Default for TypeAnalyzerAttribute {
    fn default() -> Self {
        Self {
            scope: Scope::default(),
            derived_type: None,
        }
    }
}

pub struct TypeAnalyzer {
    attribute_stack: Vec<TypeAnalyzerAttribute>,
}

impl TypeAnalyzer {
    pub fn new() -> Self {
        Self {
            attribute_stack: vec![],
        }
    }

    pub fn analyze(&mut self, ast: &mut dyn AstNode, scope: Scope) {
        self.push(TypeAnalyzerAttribute::new());
        self.top_mut().scope = scope;
        ast.accept_mut(self);
        self.pop();
    }

    fn top_mut(&mut self) -> &mut TypeAnalyzerAttribute {
        self.attribute_stack.last_mut().unwrap()
    }

    fn push(&mut self, attr: TypeAnalyzerAttribute) {
        self.attribute_stack.push(attr)
    }

    fn pop(&mut self) -> TypeAnalyzerAttribute {
        self.attribute_stack
            .pop()
            .expect("TypeAnalyzer attribute stack is empty!")
    }
}

impl AstVisitorMut for TypeAnalyzer {
    fn visit_literal_mut(&mut self, literal: &mut LiteralType) {
        todo!()
    }

    fn visit_variable_mut(&mut self, variable: &mut Variable) {
        if let Some(v) = self.top_mut().scope.find_variable(variable.name()) {
            variable.set_ty(v.ty().map(|x| x.clone()))
        }
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
        // collect all operands type
        let mut operands_attr = vec![];
        for operand in expr.operands_mut() {
            self.push(TypeAnalyzerAttribute::new());
            operand.accept_mut(self);
            operands_attr.push(self.pop());
        }
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        todo!()
    }

    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression) {
        todo!()
    }
}
