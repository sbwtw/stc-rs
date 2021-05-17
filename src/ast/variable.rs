use crate::ast::*;
use crate::parser::StString;
use std::default::Default;
use std::sync::Arc;

#[derive(Debug)]
pub struct Variable {
    name: StString,
    ty: Option<Arc<Box<dyn Type>>>,
    scope: VariableScopeClass,
    retain_flags: VariableAnnotationFlags,
}

impl Variable {
    pub fn new(name: StString) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    pub fn with_type(name: StString, ty: Arc<Box<dyn Type>>) -> Self {
        Self {
            name,
            ty: Some(ty),
            ..Default::default()
        }
    }

    /// comma split variable declare list, like: a, b, c: INT;
    pub fn multiple_variable_with_type(
        names: Vec<StString>,
        ty: Arc<Box<dyn Type>>,
    ) -> Vec<Arc<Self>> {
        names
            .iter()
            .map(|x| Arc::new(Self::with_type(x.clone(), ty.clone())))
            .collect()
    }

    pub fn set_scope(&mut self, scope: VariableScopeClass) {
        self.scope = scope
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn origin_name(&self) -> &String {
        self.name.origin_string()
    }

    pub fn ty(&self) -> Option<Arc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Arc<Box<dyn Type>>>) {
        self.ty = ty
    }

    pub fn scope_class(&self) -> &VariableScopeClass {
        &self.scope
    }

    pub fn annotation(&self) -> VariableAnnotationFlags {
        self.retain_flags
    }

    pub fn set_annotation(&mut self, flags: VariableAnnotationFlags) {
        self.retain_flags = flags
    }
}

impl AstNode for Variable {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_variable(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_variable_mut(self)
    }
}

impl Expression for Variable {}

impl Default for Variable {
    fn default() -> Self {
        Self {
            name: StString::new(""),
            ty: None,
            scope: VariableScopeClass::None,
            retain_flags: VariableAnnotationFlags::NONE,
        }
    }
}

pub struct VariableDeclareGroup;

impl VariableDeclareGroup {
    pub fn new(
        scope: VariableScopeClass,
        flags: Option<VariableAnnotationFlags>,
        mut vars: Vec<Arc<Variable>>,
    ) -> Vec<Arc<Variable>> {
        for v in vars.iter_mut() {
            Arc::get_mut(v).unwrap().set_scope(scope);
            Arc::get_mut(v)
                .unwrap()
                .set_annotation(flags.unwrap_or(VariableAnnotationFlags::NONE));
        }

        vars
    }
}
