use crate::ast::*;

#[derive(Debug)]
pub struct IfStatement {
    condition: Box<dyn Expression>,
    then_controlled: Option<Box<dyn Statement>>,
    else_controlled: Option<Box<dyn Statement>>,
    else_if_list: Vec<ElseIf>,
}

impl IfStatement {
    pub fn new(condition: Box<dyn Expression>) -> Self {
        Self {
            condition,
            then_controlled: None,
            else_controlled: None,
            else_if_list: vec![],
        }
    }

    pub fn from_then(condition: Box<dyn Expression>, then_control: Box<dyn Statement>) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: None,
            else_if_list: vec![],
        }
    }

    pub fn from_then_else(condition: Box<dyn Expression>, then_control: Box<dyn Statement>, else_control: Box<dyn Statement>) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: Some(else_control),
            else_if_list: vec![],
        }
    }

    pub fn condition(&self) -> &Box<dyn Expression> {
        &self.condition
    }

    pub fn then_controlled(&self) -> Option<&Box<dyn Statement>> {
        self.then_controlled.as_ref()
    }

    pub fn else_controlled(&self) -> Option<&Box<dyn Statement>> {
        self.else_controlled.as_ref()
    }
}

#[derive(Debug)]
pub struct ElseIf {
    condition: Box<dyn Expression>,
    then_controlled: Option<Box<dyn Statement>>,
}

impl ElseIf {
    pub fn new(condition: Box<dyn Expression>) -> Self {
        Self {
            condition,
            then_controlled: None,
        }
    }

    pub fn from_then(condition: Box<dyn Expression>, then_control: Box<dyn Statement>) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
        }
    }
}

impl AstNode for IfStatement {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_if_statement(self);
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_if_statement_mut(self);
    }
}

impl Statement for IfStatement {}