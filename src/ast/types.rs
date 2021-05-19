use crate::ast::*;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub struct IntType;

impl IntType {
    pub fn new() -> Self {
        Self {}
    }
}

impl Type for IntType {
    fn type_class(&self) -> TypeClass {
        TypeClass::Int
    }
}

#[derive(Debug, Clone)]
pub struct UserType(StString);

impl UserType {
    pub fn from_name(name: StString) -> Self {
        Self(name)
    }

    pub fn name(&self) -> &StString {
        &self.0
    }
}

impl Type for UserType {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.0.clone(), None)
    }
}

#[derive(Debug, Clone)]
pub struct EnumField {
    name: StString,
    value: Option<LiteralValue>,
}

impl EnumField {
    pub fn new(name: StString, value: Option<LiteralValue>) -> Self {
        Self { name, value }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn value(&self) -> Option<&LiteralValue> {
        self.value.as_ref()
    }
}

#[derive(Debug, Clone)]
pub struct EnumDeclare {
    name: StString,
    ty: Option<Arc<Box<dyn Type>>>,
    fields: Vec<EnumField>,
}

impl EnumDeclare {
    pub fn new(name: StString, ty: Option<Arc<Box<dyn Type>>>, fields: Vec<EnumField>) -> Self {
        Self { name, ty, fields }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn ty(&self) -> Option<Arc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn fields(&self) -> &Vec<EnumField> {
        &self.fields
    }
}

impl Type for EnumDeclare {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Enum))
    }
}

impl Declaration for EnumDeclare {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn DeclarationVisitor) {
        visitor.visit_enum_declare(self)
    }

    fn identifier(&self) -> &StString {
        self.name()
    }
}
