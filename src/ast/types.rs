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

    pub fn name(&self) -> &StString {
        &self.0
    }

    pub fn user_type_class(&self) -> Option<&UserTypeClass> {
        self.1.as_ref()
    }

    pub fn set_user_type_class(&mut self, type_class: Option<UserTypeClass>) {
        self.1 = type_class
    }
}

impl Type for UserType {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.0.clone(), self.1.clone())
    }
}
