use crate::ast::visitor::{Accept, AcceptMut};
use crate::ast::*;
use crate::{impl_ast_display, impl_into_statement};

#[derive(Debug)]
pub struct IfStatement {
    condition: Expression,
    then_controlled: Option<Statement>,
    else_controlled: Option<Statement>,
    else_if_list: Vec<ElseIfStatement>,
}

impl_ast_display!(IfStatement, visit_if_statement);
impl_into_statement!(IfStatement, |x| Statement::if_stmt(Box::new(x)));

impl IfStatement {
    #[allow(unused)]
    pub fn new(condition: Expression) -> Self {
        Self {
            condition,
            then_controlled: None,
            else_controlled: None,
            else_if_list: vec![],
        }
    }

    pub fn from_then(condition: Expression, then_control: Statement) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: None,
            else_if_list: vec![],
        }
    }

    pub fn from_then_else(
        condition: Expression,
        then_control: Statement,
        else_control: Statement,
    ) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: Some(else_control),
            else_if_list: vec![],
        }
    }

    pub fn from_then_elseif_else(
        condition: Expression,
        then_control: Statement,
        else_if_list: Vec<ElseIfStatement>,
        else_control: Statement,
    ) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
            else_controlled: Some(else_control),
            else_if_list,
        }
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn then_controlled(&self) -> Option<&Statement> {
        self.then_controlled.as_ref()
    }

    pub fn then_controlled_mut(&mut self) -> Option<&mut Statement> {
        self.then_controlled.as_mut()
    }

    pub fn else_controlled(&self) -> Option<&Statement> {
        self.else_controlled.as_ref()
    }

    pub fn else_controlled_mut(&mut self) -> Option<&mut Statement> {
        self.else_controlled.as_mut()
    }

    pub fn else_if_list(&self) -> &Vec<ElseIfStatement> {
        &self.else_if_list
    }

    pub fn else_if_list_mut(&mut self) -> &mut Vec<ElseIfStatement> {
        &mut self.else_if_list
    }
}

#[derive(Debug)]
pub struct ElseIfStatement {
    condition: Expression,
    then_controlled: Option<Statement>,
}

impl ElseIfStatement {
    #[allow(unused)]
    pub fn new(condition: Expression) -> Self {
        Self {
            condition,
            then_controlled: None,
        }
    }

    #[allow(unused)]
    pub fn from_then(condition: Expression, then_control: Statement) -> Self {
        Self {
            condition,
            then_controlled: Some(then_control),
        }
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn condition_mut(&mut self) -> &mut Expression {
        &mut self.condition
    }

    pub fn then_controlled(&self) -> Option<&Statement> {
        self.then_controlled.as_ref()
    }

    pub fn then_controlled_mut(&mut self) -> Option<&mut Statement> {
        self.then_controlled.as_mut()
    }
}
