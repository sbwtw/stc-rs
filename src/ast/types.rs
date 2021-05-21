use crate::ast::*;
use std::sync::Arc;

macro_rules! builtin_type_impl {
    (struct $name:ident, $class:expr) => {
        #[derive(Debug, Clone)]
        pub struct $name;

        impl $name {
            pub fn new() -> Self {
                Self {}
            }
        }

        impl Type for $name {
            fn type_class(&self) -> TypeClass {
                $class
            }
        }
    };
}

builtin_type_impl!(struct BitType, TypeClass::Bit);
builtin_type_impl!(struct BoolType, TypeClass::Bool);
builtin_type_impl!(struct ByteType, TypeClass::Byte);
builtin_type_impl!(struct SIntType, TypeClass::SInt);
builtin_type_impl!(struct IntType, TypeClass::Int);
builtin_type_impl!(struct UIntType, TypeClass::UInt);
builtin_type_impl!(struct DIntType, TypeClass::DInt);
builtin_type_impl!(struct UDIntType, TypeClass::UDInt);
builtin_type_impl!(struct LIntType, TypeClass::LInt);
builtin_type_impl!(struct ULIntType, TypeClass::ULInt);
builtin_type_impl!(struct RealType, TypeClass::Real);
builtin_type_impl!(struct StringType, TypeClass::String);

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
