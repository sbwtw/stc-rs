use crate::ast::*;

#[derive(Debug)]
pub struct IfStatement {
    condition: Box<dyn Expression>,
    then_controlled: Option<Box<dyn Statement>>,
    else_controlled: Option<Box<dyn Statement>>,
    else_if_list: Vec<ElseIfStatement>,
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

    pub fn from_then_else(
        condition: Box<dyn Expression>,
        then_control: Box<dyn Statement>,
        else_control: Box<dyn Statement>,
    ) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: Some(else_control),
            else_if_list: vec![],
        }
    }

    pub fn from_then_elseif_else(
        condition: Box<dyn Expression>,
        then_control: Box<dyn Statement>,
        else_if_list: Vec<ElseIfStatement>,
        else_control: Box<dyn Statement>,
    ) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: Some(else_control),
            else_if_list,
        }
    }

    pub fn condition(&self) -> &dyn Expression {
        self.condition.as_ref()
    }

    pub fn then_controlled(&self) -> Option<&Box<dyn Statement>> {
        self.then_controlled.as_ref()
    }

    pub fn else_controlled(&self) -> Option<&Box<dyn Statement>> {
        self.else_controlled.as_ref()
    }

    pub fn else_if_list(&self) -> &Vec<ElseIfStatement> {
        &self.else_if_list
    }
}

#[derive(Debug)]
pub struct ElseIfStatement {
    condition: Box<dyn Expression>,
    then_controlled: Option<Box<dyn Statement>>,
}

impl ElseIfStatement {
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

    pub fn condition(&self) -> &dyn Expression {
        self.condition.as_ref()
    }

    pub fn then_controlled(&self) -> Option<&Box<dyn Statement>> {
        self.then_controlled.as_ref()
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
