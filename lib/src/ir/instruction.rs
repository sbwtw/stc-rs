use crate::ir::Value;

pub enum OperandType {
    // +
    Add,
    // -
    Sub,
    // *
    Mul,
}

pub struct Instruction {}

impl Value for Instruction {}
