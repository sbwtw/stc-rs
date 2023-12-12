use crate::ast::*;
use crate::impl_has_attribute;
use smallmap::Map;
use std::rc::Rc;

macro_rules! builtin_type_impl {
    (struct $name:ident, $class:expr) => {
        #[derive(Debug, Clone, Default)]
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
builtin_type_impl!(struct LRealType, TypeClass::LReal);
builtin_type_impl!(struct StringType, TypeClass::String);

#[derive(Debug)]
pub struct UserType {
    name: StString,
    #[allow(dead_code)]
    decl_id: Option<usize>,
    class: Option<UserTypeClass>,
    attributes: Map<StString, String>,
}

impl UserType {
    pub fn from_name(name: StString) -> Self {
        Self {
            name,
            decl_id: None,
            class: None,
            attributes: Map::new(),
        }
    }

    pub fn from_proto(name: StString, proto: usize) -> Self {
        Self {
            name,
            decl_id: Some(proto),
            class: None,
            attributes: Map::new(),
        }
    }

    #[allow(unused)]
    pub fn name(&self) -> &StString {
        &self.name
    }
}

impl Type for UserType {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.name.clone(), self.class.clone())
    }
}

impl_has_attribute!(UserType, attributes);

#[derive(Debug)]
pub struct EnumDeclare {
    name: StString,
    ty: Option<Rc<Box<dyn Type>>>,
    fields: SmallVec8<Rc<Variable>>,
}

impl EnumDeclare {
    pub fn new(
        name: StString,
        ty: Option<Rc<Box<dyn Type>>>,
        fields: SmallVec8<Rc<Variable>>,
    ) -> Self {
        Self { name, ty, fields }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    pub fn fields(&self) -> &[Rc<Variable>] {
        &self.fields
    }
}

impl Type for EnumDeclare {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Enum))
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct AliasDeclare {
    name: StString,
    alias: Rc<Box<dyn Type>>,
}

impl AliasDeclare {
    pub fn new(name: StString, alias: Rc<Box<dyn Type>>) -> Self {
        Self { name, alias }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }
}

impl Type for AliasDeclare {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Alias))
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct StructDeclare {
    name: StString,
    variables: SmallVec8<Rc<Variable>>,
}

impl StructDeclare {
    pub fn new(name: StString, variables: SmallVec8<Rc<Variable>>) -> Self {
        Self { name, variables }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn variables(&self) -> &[Rc<Variable>] {
        self.variables.as_slice()
    }

    #[allow(dead_code)]
    pub fn variables_mut(&mut self) -> &mut [Rc<Variable>] {
        self.variables.as_mut_slice()
    }
}

impl Type for StructDeclare {
    fn type_class(&self) -> TypeClass {
        TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Struct))
    }
}
