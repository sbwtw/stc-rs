use std::rc::Rc;

use crate::{ast::*, impl_has_attribute, utils::AttrMap8};

macro_rules! builtin_type_impl {
    (struct $name:ident, $class:expr) => {
        #[derive(Debug, Clone, Default)]
        pub struct $name;

        impl $name {
            pub fn new_type() -> Type {
                Type::from_class($class)
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UserType {
    name: StString,
    decl_id: Option<usize>,
    class: Option<UserTypeClass>,
}

impl UserType {
    pub fn from_name(name: StString) -> Self {
        Self {
            name,
            decl_id: None,
            class: None,
        }
    }

    pub fn from_proto(name: StString, proto_id: usize) -> Self {
        Self {
            name,
            decl_id: Some(proto_id),
            class: None,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }
}

impl From<UserType> for Type {
    fn from(value: UserType) -> Self {
        let class = TypeClass::UserType(RefCell::new(value));

        Type::from_class(class)
    }
}

impl Display for UserType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.name)
    }
}

#[derive(Debug)]
pub struct EnumDeclare {
    name: StString,
    ty: Option<Type>,
    fields: SmallVec8<Rc<Variable>>,
    attributes: AttrMap8,
}

impl_has_attribute!(EnumDeclare, attributes);

impl EnumDeclare {
    pub fn new(name: StString, ty: Option<Type>, fields: SmallVec8<Rc<Variable>>) -> Self {
        Self {
            name,
            ty,
            fields,
            attributes: AttrMap8::new(),
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn ty(&self) -> &Option<Type> {
        &self.ty
    }

    pub fn fields(&self) -> &[Rc<Variable>] {
        &self.fields
    }
}

// impl Type for EnumDeclare {
//     fn type_class(&self) -> TypeClass {
//         TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Enum))
//     }
// }

#[derive(Debug)]
pub struct AliasDeclare {
    name: StString,
    alias: Type,
    attributes: AttrMap8,
}

impl_has_attribute!(AliasDeclare, attributes);

impl AliasDeclare {
    pub fn new(name: StString, alias: Type) -> Self {
        Self {
            name,
            alias,
            attributes: AttrMap8::new(),
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }
}

// impl Type for AliasDeclare {
//     fn type_class(&self) -> TypeClass {
//         TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Alias))
//     }
// }

#[derive(Debug)]
pub struct StructDeclare {
    name: StString,
    variables: SmallVec8<Rc<Variable>>,
    attributes: AttrMap8,
}

impl_has_attribute!(StructDeclare, attributes);

impl StructDeclare {
    pub fn new(name: StString, variables: SmallVec8<Rc<Variable>>) -> Self {
        Self {
            name,
            variables,
            attributes: AttrMap8::new(),
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn variables(&self) -> &[Rc<Variable>] {
        self.variables.as_slice()
    }

    pub fn variables_mut(&mut self) -> &mut [Rc<Variable>] {
        self.variables.as_mut_slice()
    }
}

// impl Type for StructDeclare {
//     fn type_class(&self) -> TypeClass {
//         TypeClass::UserType(self.name.clone(), Some(UserTypeClass::Struct))
//     }
// }

#[derive(Debug)]
pub struct ArrayType {
    base_type: RefCell<Type>,
    dimensions: SmallVec3<RangeExpression>,
}

impl ArrayType {
    pub fn new(base: Type, dimensions: SmallVec3<RangeExpression>) -> Self {
        Self {
            base_type: RefCell::new(base),
            dimensions,
        }
    }
}

impl Display for ArrayType {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        todo!()
    }
}

impl PartialEq for ArrayType {
    fn eq(&self, other: &Self) -> bool {
        // self.base_type == other.base_type && self.dimensions == other.dimensions
        todo!()
    }
}

impl Eq for ArrayType {}

impl Clone for ArrayType {
    fn clone(&self) -> Self {
        todo!()
    }
}

impl From<ArrayType> for Type {
    fn from(value: ArrayType) -> Self {
        let class = TypeClass::Array(RefCell::new(value));

        Type::from_class(class)
    }
}