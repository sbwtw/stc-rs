use crate::ast::*;

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
pub struct UserType(StString, Option<UserTypeClass>);

impl UserType {
    pub fn from_name(name: StString) -> Self {
        Self(name, None)
    }
}

impl Type for UserType {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.0.clone(), self.1.clone())
    }
}
