use crate::ast::*;
use crate::impl_has_attribute;
use crate::parser::StString;
use crate::utils::AttrMap8;
use std::default::Default;
use std::rc::Rc;

#[derive(Debug)]
pub struct Variable {
    attributes: AttrMap8,
    name: StString,
    ty: Option<Type>,
    flags: VariableFlags,
    initial: Option<Box<Expression>>,
}

impl Variable {
    pub fn new(name: StString) -> Self {
        Self {
            name,
            ..Default::default()
        }
    }

    pub fn with_type(name: StString, ty: Type) -> Self {
        Self {
            name,
            ty: Some(ty),
            ..Default::default()
        }
    }

    pub fn with_initial(name: StString, initial: Option<Box<Expression>>) -> Self {
        Self {
            name,
            initial,
            ..Default::default()
        }
    }

    /// comma split variable declare list, like: a, b, c: INT;
    pub fn multiple_variable_with_type(
        names: SmallVec8<StString>,
        ty: Type,
    ) -> SmallVec8<Rc<Self>> {
        names
            .iter()
            .map(|x| Rc::new(Self::with_type(x.clone(), ty.clone())))
            .collect()
    }

    #[inline]
    pub fn name(&self) -> &StString {
        &self.name
    }

    #[inline]
    pub fn origin_name(&self) -> &String {
        self.name.origin_string()
    }

    #[inline]
    pub fn ty(&self) -> Option<&Type> {
        self.ty.as_ref()
    }

    #[inline]
    pub fn set_ty(&mut self, ty: Option<Type>) {
        self.ty = ty
    }

    #[inline]
    pub fn initial(&self) -> &Option<Box<Expression>> {
        &self.initial
    }

    #[inline]
    pub fn set_flags(&mut self, flags: VariableFlags) {
        self.flags = flags
    }

    #[inline]
    pub fn flags(&self) -> VariableFlags {
        self.flags
    }

    #[inline]
    pub fn scope(&self) -> VariableFlags {
        self.flags & (VariableFlags::INPUT | VariableFlags::INOUT | VariableFlags::OUTPUT)
    }
}

impl Default for Variable {
    fn default() -> Self {
        Self {
            attributes: AttrMap8::new(),
            name: StString::empty(),
            ty: None,
            flags: VariableFlags::NONE,
            initial: None,
        }
    }
}

impl_has_attribute!(Variable, attributes);

pub struct VariableDeclareGroup;

impl VariableDeclareGroup {
    pub fn from_variables(
        flags: VariableFlags,
        mut vars: SmallVec8<Rc<Variable>>,
    ) -> SmallVec8<Rc<Variable>> {
        for v in vars.iter_mut() {
            Rc::get_mut(v).unwrap().set_flags(flags);
        }

        vars
    }
}
