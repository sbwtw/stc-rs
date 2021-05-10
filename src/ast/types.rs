use crate::ast::*;

#[derive(Debug)]
pub struct IntType;

impl IntType {
    pub fn new() -> Self {
        Self {}
    }
}

impl Type for IntType {}

#[derive(Debug)]
pub struct UserType(StString);

impl UserType {
    pub fn from_name(name: StString) -> Self {
        Self(name)
    }
}

impl Type for UserType {}
