use crate::ast::*;
use crate::parser::StString;

#[derive(Debug)]
pub struct Variable {
    name: StString,
    ty: Option<Box<dyn Type>>,
}

impl Variable {
    pub fn new(name: StString) -> Self {
        Self { name, ty: None }
    }

    pub fn with_type(name: StString, ty: Box<dyn Type>) -> Self {
        Self { name, ty: Some(ty) }
    }

    /// comma split variable declare list, like: a, b, c: INT;
    pub fn multiple_variable_with_type(names: Vec<StString>, ty: Box<dyn Type>) -> Vec<Self> {
        names.iter().map(|x| Self::with_type(x.clone(), ty.clone())).collect()
    }

    pub fn origin_name(&self) -> &String {
        self.name.origin_string()
    }

    pub fn ty(&self) -> Option<&Box<dyn Type>> {
        self.ty.as_ref()
    }
}

impl AstNode for Variable {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_variable(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_variable(self)
    }
}

impl Expression for Variable {}
