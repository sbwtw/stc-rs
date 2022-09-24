use crate::ast::*;
use crate::parser::LiteralValue;

#[derive(Debug, Clone)]
pub struct LiteralExpression(LiteralValue);

impl LiteralExpression {
    pub fn new(val: LiteralValue) -> Self {
        LiteralExpression(val)
    }

    pub fn literal(&self) -> &LiteralValue {
        &self.0
    }
}
