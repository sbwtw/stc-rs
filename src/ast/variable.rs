use crate::ast::*;
use crate::impl_has_attribute;
use crate::parser::StString;
use std::collections::BTreeMap;
use std::default::Default;
use std::rc::Rc;

#[derive(Debug)]
pub struct Variable {
    attributes: BTreeMap<StString, String>,
    name: StString,
    ty: Option<Rc<Box<dyn Type>>>,
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

    pub fn with_type(name: StString, ty: Rc<Box<dyn Type>>) -> Self {
        Self {
            name,
            ty: Some(ty),
            ..Default::default()
        }
    }

    /// comma split variable declare list, like: a, b, c: INT;
    pub fn multiple_variable_with_type(
        names: Vec<StString>,
        ty: Rc<Box<dyn Type>>,
    ) -> Vec<Rc<Self>> {
        names
            .iter()
            .map(|x| Rc::new(Self::with_type(x.clone(), ty.clone())))
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

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn set_ty(&mut self, ty: Option<Rc<Box<dyn Type>>>) {
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
            attributes: BTreeMap::new(),
            name: StString::new(""),
            ty: None,
            scope: VariableScopeClass::None,
            retain_flags: VariableAnnotationFlags::NONE,
        }
    }
}

impl_has_attribute!(Variable, attributes);

pub struct VariableDeclareGroup;

impl VariableDeclareGroup {
    pub fn new(
        scope: VariableScopeClass,
        flags: Option<VariableAnnotationFlags>,
        mut vars: Vec<Rc<Variable>>,
    ) -> Vec<Rc<Variable>> {
        for v in vars.iter_mut() {
            Rc::get_mut(v).unwrap().set_scope(scope);
            Rc::get_mut(v)
                .unwrap()
                .set_annotation(flags.unwrap_or(VariableAnnotationFlags::NONE));
        }

        vars
    }
}
