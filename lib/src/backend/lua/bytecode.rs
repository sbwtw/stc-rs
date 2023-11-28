use crate::backend::lua::encoding::LuaOpCode;
use crate::backend::TargetCode;

use indexmap::IndexSet;
use std::fmt::{Display, Formatter};

pub struct LuaExecState {}

#[derive(PartialEq, Debug, Clone, Hash, Eq)]
pub enum LuaConstants {
    Nil,
    String(String),
    Integer(i64),
    Function(fn(&mut LuaExecState) -> i32),
}

#[derive(Debug)]
pub enum LuaByteCode {
    /// Call k, v: k is callee symbol position, v is argument count, return value not included
    /// A B C: R[A], ... ,R[A+C-2] := R[A](R[A+1], ... ,R[A+B-1])
    Call(u8, u8, u8),
    /// A B C: R[A] := UpValue[B][K[C]:string]
    GetTabUp(u8, u8, u8),
    /// A B C: UpValue[A][K[B]:string] := RK(C)
    SetTabUp(u8, u8, u8),
}

impl LuaByteCode {
    fn mnemonic(&self) -> &'static str {
        match self {
            LuaByteCode::Call(_, _, _) => "CALL",
            LuaByteCode::GetTabUp(_, _, _) => "GETTABUP",
            LuaByteCode::SetTabUp(_, _, _) => "SETTABUP",
        }
    }

    fn opcode(&self) -> LuaOpCode {
        match self {
            LuaByteCode::Call(_, _, _) => LuaOpCode::OP_CALL,
            LuaByteCode::GetTabUp(_, _, _) => LuaOpCode::OP_GETTABUP,
            LuaByteCode::SetTabUp(_, _, _) => LuaOpCode::OP_SETTABUP,
        }
    }
}

impl Display for LuaByteCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            LuaByteCode::Call(a, b, c) | LuaByteCode::GetTabUp(a, b, c) => {
                write!(f, "{:<10} {a} {b} {c}", self.mnemonic())
            }
            LuaByteCode::SetTabUp(a, b, c) => write!(f, "{:<10} {a} {b} {c}k", self.mnemonic()),
        }
    }
}

#[derive(Debug)]
pub struct LuaCode {
    pub byte_codes: Vec<LuaByteCode>,
    pub constants: IndexSet<LuaConstants>,
}

impl Display for LuaCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // constants
        writeln!(f, "Constants:")?;
        for constant in self.constants.iter() {
            writeln!(f, "{:?}", constant)?
        }

        writeln!(f)?;

        // constants
        writeln!(f, "ByteCodes:")?;
        for (idx, bc) in self.byte_codes.iter().enumerate() {
            writeln!(f, "{idx:<6} {:<20}", bc)?
        }

        Ok(())
    }
}

impl TargetCode for LuaCode {}
