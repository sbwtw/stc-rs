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
    flags: VariableFlags,
    initial: Option<Box<Expression>>,
}

impl Variable {
    #[allow(dead_code)]
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
        ty: Rc<Box<dyn Type>>,
    ) -> SmallVec8<Rc<Self>> {
        names
            .iter()
            .map(|x| Rc::new(Self::with_type(x.clone(), ty.clone())))
            .collect()
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    #[allow(dead_code)]
    pub fn origin_name(&self) -> &String {
        self.name.origin_string()
    }

    pub fn ty(&self) -> Option<Rc<Box<dyn Type>>> {
        self.ty.clone()
    }

    #[allow(dead_code)]
    pub fn set_ty(&mut self, ty: Option<Rc<Box<dyn Type>>>) {
        self.ty = ty
    }

    pub fn initial(&self) -> &Option<Box<Expression>> {
        &self.initial
    }

    pub fn set_flags(&mut self, flags: VariableFlags) {
        self.flags = flags
    }

    pub fn flags(&self) -> VariableFlags {
        self.flags
    }

    pub fn scope(&self) -> VariableFlags {
        self.flags & (VariableFlags::INPUT | VariableFlags::INOUT | VariableFlags::OUTPUT)
    }
}

impl Default for Variable {
    fn default() -> Self {
        Self {
            attributes: BTreeMap::new(),
            name: StString::new(""),
            ty: None,
            flags: VariableFlags::NONE,
            initial: None,
        }
    }
}

impl_has_attribute!(Variable, attributes);

pub struct VariableDeclareGroup;

impl VariableDeclareGroup {
    pub fn new(flags: VariableFlags, mut vars: SmallVec8<Rc<Variable>>) -> SmallVec8<Rc<Variable>> {
        for v in vars.iter_mut() {
            Rc::get_mut(v).unwrap().set_flags(flags);
        }

        vars
    }
}
