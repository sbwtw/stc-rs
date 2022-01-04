use crate::ast::Variable;
use crate::StString;
use std::rc::Rc;

#[derive(Debug)]
pub struct GlobalVariableDeclare {
    name: StString,
    variables: Vec<Rc<Variable>>,
}

impl GlobalVariableDeclare {
    pub fn new(name: Option<StString>, variables: Vec<Rc<Variable>>) -> Self {
        Self {
            name: name.unwrap_or(StString::empty()),
            variables,
        }
    }

    pub fn name(&self) -> &StString {
        &self.name
    }

    pub fn variables(&self) -> &Vec<Rc<Variable>> {
        &self.variables
    }
}
