use crate::ast::*;
use crate::context::Scope;
use std::rc::Rc;

struct TypeAnalyzerAttribute {
    derived_type: Option<Rc<Box<dyn Type>>>,
}

impl TypeAnalyzerAttribute {
    pub fn new() -> Self {
        Default::default()
    }
}

impl Default for TypeAnalyzerAttribute {
    fn default() -> Self {
        Self { derived_type: None }
    }
}

pub struct TypeAnalyzer {
    scope: Scope,
    attribute_stack: Vec<TypeAnalyzerAttribute>,
}

impl TypeAnalyzer {
    pub fn new() -> Self {
        Self {
            scope: Scope::default(),
            attribute_stack: vec![],
        }
    }

    pub fn analyze_statement(&mut self, stmt: &mut Statement, scope: Scope) {
        self.scope = scope;

        self.push(TypeAnalyzerAttribute::new());
        self.visit_statement_mut(stmt);
        self.pop();

        debug_assert_eq!(self.attribute_stack.len(), 0)
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
    fn visit_literal_mut(&mut self, literal: &mut LiteralExpression) {
        self.top_mut().derived_type = Some(Rc::new(literal.literal().ty()))
    }

    fn visit_variable_expression_mut(&mut self, variable: &mut VariableExpression) {
        if variable.ty().is_none() {
            if let Some(v) = self.scope.find_variable(variable.name()) {
                variable.set_ty(v.ty().map(|x| x.clone()))
            }
        }

        self.top_mut().derived_type = variable.ty()
    }

    fn visit_operator_expression_mut(&mut self, expr: &mut OperatorExpression) {
        // collect all operands type
        let mut operands_attr = vec![];
        for operand in expr.operands_mut() {
            self.push(TypeAnalyzerAttribute::new());
            self.visit_expression_mut(operand);
            operands_attr.push(self.pop());
        }

        let result_type = &mut self.top_mut().derived_type;
        for attr in operands_attr {
            if let Some(true) = attr
                .derived_type
                .as_deref()
                .zip(result_type.as_deref())
                .map(|(t1, t2)| t1.type_class() == t2.type_class())
            {
                continue;
            }

            if result_type.is_none() {
                *result_type = attr.derived_type.clone();
                continue;
            }
        }

        expr.set_ty(result_type.clone());
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        self.push(TypeAnalyzerAttribute::new());
        self.visit_expression_mut(assign.right_mut());
        self.pop();

        self.push(TypeAnalyzerAttribute::new());
        self.visit_expression_mut(assign.left_mut());
        let attr = self.pop();

        assign.set_ty(attr.derived_type)
    }

    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression) {
        self.push(TypeAnalyzerAttribute::new());
        self.visit_expression_mut(compo.left_mut());
        self.pop();

        self.push(TypeAnalyzerAttribute::new());
        self.visit_expression_mut(compo.right_mut());
        let attr = self.pop();

        compo.set_ty(attr.derived_type.clone());
        self.top_mut().derived_type = attr.derived_type
    }
}
