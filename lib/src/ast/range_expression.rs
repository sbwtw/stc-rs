use super::{Expression, SmallVec3};

/// Lower..Upper
#[derive(Debug)]
pub struct RangeExpression {
    lower: Expression,
    upper: Expression,
}

impl RangeExpression {
    pub fn new(lower: Expression, upper: Expression) -> Self {
        Self { lower, upper }
    }
}

pub type Dimensions = SmallVec3<RangeExpression>;
