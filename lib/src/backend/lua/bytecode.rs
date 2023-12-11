use crate::backend::lua::encoding::LuaOpCode;
use crate::backend::TargetCode;

use indexmap::IndexSet;
use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};

pub struct LuaExecState {}

#[derive(PartialEq, Debug, Clone)]
pub enum LuaConstants {
    Nil,
    String(String),
    Integer(i64),
    Float(f64),
    Function(fn(&mut LuaExecState) -> i32),
}

impl Eq for LuaConstants {}

impl Hash for LuaConstants {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LuaConstants::Float(f) => state.write(&f.to_be_bytes()),
            LuaConstants::Function(f) => f.hash(state),
            LuaConstants::Nil => 0.hash(state),
            LuaConstants::String(s) => s.hash(state),
            LuaConstants::Integer(i) => i.hash(state),
        }
    }
}

impl Display for LuaConstants {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            LuaConstants::String(ref s) => write!(f, "{s}"),
            LuaConstants::Float(ref v) => write!(f, "{:?}", v),
            _ => todo!(),
        }
    }
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
    /// A B: R[A] := K[Bx]
    LoadK(u8, u32),
}

impl LuaByteCode {
    fn mnemonic(&self) -> &'static str {
        match self {
            LuaByteCode::Call(..) => "CALL",
            LuaByteCode::GetTabUp(..) => "GETTABUP",
            LuaByteCode::SetTabUp(..) => "SETTABUP",
            LuaByteCode::LoadK(..) => "LOADK",
        }
    }

    fn opcode(&self) -> LuaOpCode {
        match self {
            LuaByteCode::Call(..) => LuaOpCode::OP_CALL,
            LuaByteCode::GetTabUp(..) => LuaOpCode::OP_GETTABUP,
            LuaByteCode::SetTabUp(..) => LuaOpCode::OP_SETTABUP,
            LuaByteCode::LoadK(..) => LuaOpCode::OP_LOADK,
        }
    }
}

#[derive(Debug)]
pub struct LuaCode {
    pub byte_codes: Vec<LuaByteCode>,
    pub constants: IndexSet<LuaConstants>,
}

impl LuaCode {
    pub fn disassembly_code(&self, code: &LuaByteCode) -> String {
        let mut s = String::with_capacity(16);
        write!(s, "{:<10} ", code.mnemonic()).unwrap();

        match code {
            // ABC
            LuaByteCode::Call(a, b, c)
            | LuaByteCode::GetTabUp(a, b, c)
            | LuaByteCode::SetTabUp(a, b, c) => {
                write!(s, "{a} {b} {c}").unwrap();
            }
            // ABx
            LuaByteCode::LoadK(a, bx) => {
                write!(s, "{a} {bx}").unwrap();
            }
        }

        match code {
            LuaByteCode::LoadK(a, bx) => {
                write!(s, " ; {}", self.constants[*bx as usize]).unwrap();
            }
            LuaByteCode::GetTabUp(a, b, c) => {
                write!(s, " ; _ENV \"{}\"", self.constants[*b as usize]).unwrap();
            }
            LuaByteCode::Call(a, b, c) => {
                write!(s, " ; {b} in {c} out").unwrap();
            }
            _ => {}
        }

        s
    }
}

impl Display for LuaCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // constants
        writeln!(f, "Constants:")?;
        for (idx, constant) in self.constants.iter().enumerate() {
            writeln!(f, "{:<3} {:?}", idx, constant)?
        }

        writeln!(f)?;

        // constants
        writeln!(f, "ByteCodes:")?;
        for (idx, bc) in self.byte_codes.iter().enumerate() {
            writeln!(f, "{:<6} {:<20}", idx + 1, self.disassembly_code(bc))?
        }

        Ok(())
    }
}

impl TargetCode for LuaCode {}
