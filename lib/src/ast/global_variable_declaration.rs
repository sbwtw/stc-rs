use crate::ast::{SmallVec8, Variable};
use crate::impl_has_attribute;
use crate::parser::StString;
use crate::utils::AttrMap8;
use std::rc::Rc;

#[derive(Debug)]
pub struct GlobalVariableDeclare {
    name: StString,
    variables: SmallVec8<Rc<Variable>>,
    attributes: AttrMap8,
}

impl_has_attribute!(GlobalVariableDeclare, attributes);

impl GlobalVariableDeclare {
    pub fn new(name: Option<StString>, variables: SmallVec8<Rc<Variable>>) -> Self {
        Self {
            name: name.unwrap_or(StString::empty()),
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
}
