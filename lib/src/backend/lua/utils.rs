use crate::parser::{BitValue, LiteralValue};

/// sBx use 17 Bits
const SBX_BIT_SIZE: u32 = 17;
/// Max value of sBx is 2^18 - 1
const SBX_MAX_VALUE: i32 = 2_i32.pow(SBX_BIT_SIZE + 1) - 1;
/// Min value of sBx is -2^18
const SBX_MIN_VALUE: i32 = 0 - 2_i32.pow(SBX_BIT_SIZE);
/// sBx Mask, lower 17 Bits is 1
const SBX_MASK: u32 = 0b0001_1111_1111_1111_1111;

/// Returns true if literal can fit into sBx value
pub fn try_fit_sbx(literal: &LiteralValue) -> Option<u32> {
    match literal {
        LiteralValue::Bit(BitValue::Zero) => Some(0),
        LiteralValue::Bit(BitValue::One) => Some(1),
        LiteralValue::Bool(false) => Some(0),
        LiteralValue::Bool(true) => Some(1),
        LiteralValue::Byte(v) => Some(*v as u32),
        LiteralValue::SInt(v) => Some((*v as i32) as u32 & SBX_MASK),
        LiteralValue::Int(v) => Some((*v as i32) as u32 & SBX_MASK),
        LiteralValue::UInt(v) => Some(*v as u32 & SBX_MASK),
        LiteralValue::DInt(v) => {
            if (SBX_MIN_VALUE..=SBX_MAX_VALUE).contains(v) {
                Some(*v as u32 & SBX_MASK)
            } else {
                None
            }
        }
        LiteralValue::UDInt(v) => {
            if *v <= SBX_MAX_VALUE as u32 {
                Some(v & SBX_MASK)
            } else {
                None
            }
        }
        LiteralValue::LInt(v) => {
            if (SBX_MIN_VALUE as i64..=SBX_MAX_VALUE as i64).contains(v) {
                Some(*v as u32 & SBX_MASK)
            } else {
                None
            }
        }
        LiteralValue::ULInt(v) => {
            if *v <= SBX_MAX_VALUE as u64 {
                Some(*v as u32 & SBX_MASK)
            } else {
                None
            }
        }
        _ => None,
    }
}
