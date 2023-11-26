use crate::prelude::*;
use crate::{impl_ast_display, impl_into_expression};
use smallvec::smallvec;

#[derive(Debug)]
pub struct CallExpression {
    callee: Expression,
    arguments: SmallVec8<Expression>,
}

impl_ast_display!(CallExpression, visit_call_expression);
impl_into_expression!(CallExpression, |x| Expression::call(Box::new(x)));

impl CallExpression {
    pub fn new(callee: Expression) -> Self {
        Self {
            callee,
            arguments: smallvec![],
        }
    }

    pub fn with_arguments(callee: Expression, arguments: SmallVec8<Expression>) -> Self {
        Self { callee, arguments }
    }

    pub fn callee(&self) -> &Expression {
        &self.callee
    }

    pub fn arguments(&self) -> &[Expression] {
        self.arguments.as_slice()
    }
}
