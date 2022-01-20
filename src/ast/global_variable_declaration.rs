use crate::ast::{SmallVec8, Variable};
use crate::StString;
use std::rc::Rc;

#[derive(Debug)]
pub struct GlobalVariableDeclare {
    name: StString,
    variables: SmallVec8<Rc<Variable>>,
}

impl GlobalVariableDeclare {
    pub fn new(name: Option<StString>, variables: SmallVec8<Rc<Variable>>) -> Self {
        Self {
            name: name.unwrap_or(StString::empty()),
            variables,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn variables(&self) -> &[Rc<Variable>] {
        self.variables.as_slice()
    }
}
