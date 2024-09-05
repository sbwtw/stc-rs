use crate::ast::*;
use crate::context::Scope;

/// Type analysis attribute
#[derive(Clone, Default)]
struct TypeAnalyzerAttribute {
    scope: Option<Scope>,
    search_local_only: bool,
    /// the analysis result of current type
    derived_type: Option<Type>,
}

#[derive(Default)]
pub struct TypeAnalyzer {
    local_scope: Scope,
    attribute_stack: Vec<TypeAnalyzerAttribute>,
}

impl TypeAnalyzer {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn analyze_statement(&mut self, stmt: &mut Statement, scope: Scope) {
        self.local_scope = scope;

        self.push(TypeAnalyzerAttribute::default());
        self.visit_statement_mut(stmt);
        self.pop();

        debug_assert_eq!(self.attribute_stack.len(), 0)
    }

    fn current_scope(&self) -> &Scope {
        self.attribute_stack
            .last()
            .and_then(|x| x.scope.as_ref())
            .unwrap_or(&self.local_scope)
    }

    #[inline]
    fn top(&self) -> &TypeAnalyzerAttribute {
        self.attribute_stack.last().unwrap()
    }

    #[inline]
    fn top_mut(&mut self) -> &mut TypeAnalyzerAttribute {
        self.attribute_stack.last_mut().unwrap()
    }

    #[inline]
    fn push(&mut self, attr: TypeAnalyzerAttribute) {
        self.attribute_stack.push(attr)
    }

    #[inline]
    fn push_default(&mut self) {
        self.push(TypeAnalyzerAttribute::default())
    }

    fn pop(&mut self) -> TypeAnalyzerAttribute {
        self.attribute_stack
            .pop()
            .expect("TypeAnalyzer attribute stack is empty!")
    }
}

impl AstVisitorMut for TypeAnalyzer {
    fn visit_literal_mut(&mut self, literal: &mut LiteralExpression) {
        self.top_mut().derived_type = Some(literal.literal().ty())
    }

    fn visit_variable_expression_mut(&mut self, variable: &mut VariableExpression) {
        let derived_variable = if self.top().search_local_only {
            self.current_scope().find_local_variable(variable.name())
        } else {
            self.current_scope().find_variable(variable.name())
        };
        let (derived_declaration, decl_scope) =
            self.current_scope().find_declaration(variable.name());

        let attr = self.top_mut();
        let ty = match (derived_variable, derived_declaration) {
            (Some(v), None) => v.ty().cloned(),
            (None, Some(decl)) => {
                // update scope to inner declaration
                attr.scope = decl_scope;
                decl.read().unwrap().create_user_type()
            }

            // TODO: Ambiguity ?
            _ => None,
        };

        variable.set_ty(ty.clone());
        attr.derived_type.clone_from(&ty);
    }

    fn visit_operator_expression_mut(&mut self, expr: &mut OperatorExpression) {
        // collect all operands type
        let mut operands_attr = vec![];
        for operand in expr.operands_mut() {
            self.push(TypeAnalyzerAttribute::default());
            self.visit_expression_mut(operand);
            operands_attr.push(self.pop());
        }

        // let ref mut result_type = self.top_mut().derived_type;
        // for attr in operands_attr {
        //     if let Some(true) = attr
        //         .derived_type
        //         .zip(result_type)
        //         .map(|(t1, t2)| t1.type_class() == t2.type_class())
        //     {
        //         continue;
        //     }
        //
        //     if result_type.is_none() {
        //         result_type.clone_from(&attr.derived_type);
        //         continue;
        //     }
        // }
        //
        // expr.set_ty(result_type.map(|x| x.clone()));
    }

    fn visit_assign_expression_mut(&mut self, assign: &mut AssignExpression) {
        self.push(TypeAnalyzerAttribute::default());
        self.visit_expression_mut(assign.right_mut());
        self.pop();

        self.push(TypeAnalyzerAttribute::default());
        self.visit_expression_mut(assign.left_mut());
        let attr = self.pop();

        assign.set_ty(attr.derived_type.clone())
    }

    fn visit_compo_access_expression_mut(&mut self, compo: &mut CompoAccessExpression) {
        self.push(TypeAnalyzerAttribute::default());
        self.visit_expression_mut(compo.left_mut());
        let attr = self.pop();

        self.push_default();
        self.top_mut().scope = attr.scope;
        self.top_mut().search_local_only = true;
        self.visit_expression_mut(compo.right_mut());
        let attr = self.pop();

        compo.set_ty(attr.derived_type.clone());
        self.top_mut().derived_type = attr.derived_type
    }
}
