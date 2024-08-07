use crate::parser::StString;

use indexmap::{IndexMap, IndexSet};
use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};

use super::register::{Reg, RK};
use super::{ConstIdx, LuaType, LuaVarKind};

macro_rules! excess_k {
    ($v: expr, $k: literal) => {
        ($v as u32).wrapping_add(((2u32.pow($k) - 1) / 2)) & (2u32.pow($k) - 1)
    };
}

macro_rules! excess_sbx {
    ($v: expr) => {
        excess_k!($v, 17)
    };
}

macro_rules! excess_sj {
    ($v: expr) => {
        excess_k!($v, 25)
    };
}

#[repr(u8)]
#[allow(non_camel_case_types)]
enum LuaOpCode {
    // A B, R[A] := R[B]
    OP_MOVE = 0,
    // A sBx, R[A] := sBx
    OP_LOADI = 1,
    // A sBx, R[A] := (lua_Number)xBx
    OP_LOADF = 2,
    // A Bx, R[A] := K[Bx]
    OP_LOADK = 3,
    // A, R[A] := K[extra arg]
    OP_LOADKX = 4,
    OP_LOADFALSE = 5,
    OP_LFALSESKIP = 6,
    OP_LOADTRUE = 7,
    OP_LOADNIL = 8,
    OP_GETUPVAL = 9,
    OP_SETUPVAL = 10,

    OP_GETTABUP = 11,
    OP_GETTABLE = 12,
    OP_GETI = 13,
    OP_GETFIELD = 14,

    OP_SETTABUP = 15,
    OP_SETTABLE = 16,
    OP_SETI = 17,
    OP_SETFIELD = 18,

    OP_NEWTABLE = 19,

    OP_SELF = 20,

    OP_ADDI = 21,

    OP_ADDK = 22,
    OP_SUBK = 23,
    OP_MULK = 24,
    OP_MODK = 25,
    OP_POWK = 26,
    OP_DIVK = 27,
    OP_IDIVK = 28,

    OP_BANDK = 29,
    OP_BORK = 30,
    OP_BXORK = 31,

    OP_SHRI = 32,
    OP_SHLI = 33,

    OP_ADD = 34,
    OP_SUB = 35,
    OP_MUL = 36,
    OP_MOD = 37,
    OP_POW = 38,
    OP_DIV = 39,
    OP_IDIV = 40,

    OP_BAND = 41,
    OP_BOR = 42,
    OP_BXOR = 43,
    OP_SHL = 44,
    OP_SHR = 45,

    OP_MMBIN = 46,
    OP_MMBINI = 47,
    OP_MMBINK = 48,

    OP_UNM = 49,
    OP_BNOT = 50,
    OP_NOT = 51,
    OP_LEN = 52,

    OP_CONCAT = 53,

    OP_CLOSE = 54,
    OP_TBC = 55,
    OP_JMP = 56,
    OP_EQ = 57,
    OP_LT = 58,
    OP_LE = 59,

    OP_EQK = 60,
    OP_EQI = 61,
    OP_LTI = 62,
    OP_LEI = 63,
    OP_GTI = 64,
    OP_GEI = 65,

    OP_TEST = 66,
    OP_TESTSET = 67,

    OP_CALL = 68,
    OP_TAILCALL = 69,

    OP_RETURN = 70,
    OP_RETURN0 = 71,
    OP_RETURN1 = 72,

    OP_FORLOOP = 73,
    OP_FORPREP = 74,

    OP_TFORPREP = 75,
    OP_TFORCALL = 76,
    OP_TFORLOOP = 77,

    OP_SETLIST = 78,

    OP_CLOSURE = 79,

    OP_VARARG = 80,

    OP_VARARGPREP = 81,

    OP_EXTRAARG = 82,
}

pub struct LuaExecState {}

#[derive(PartialEq, Debug, Clone)]
pub enum LuaConstants {
    Nil,
    String(String),
    Integer(i64),
    Float(f64),
    Function(fn(&mut LuaExecState) -> i32),
}

impl LuaConstants {
    pub fn lua_type(&self) -> LuaType {
        match *self {
            Self::Nil => LuaType::NIL,
            Self::Function(..) => LuaType::FUNCTION,
            Self::String(..) => LuaType::STRING,
            Self::Float(..) | Self::Integer(..) => LuaType::NUMBER,
        }
    }

    pub fn as_lua_i8(&self) -> Option<i8> {
        match *self {
            Self::Integer(i) if i >= i8::MIN as i64 && i <= i8::MAX as i64 => Some(i as i8),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LuaUpValue {
    pub stack: u8,
    pub index: u8,
    pub kind: LuaVarKind,
}

impl Default for LuaUpValue {
    fn default() -> Self {
        Self {
            stack: 0,
            index: 0,
            kind: LuaVarKind::VDKREG,
        }
    }
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
            LuaConstants::Integer(ref i) => write!(f, "{}", i),
            _ => panic!("Display for constants {:?} not implement", self),
        }
    }
}

#[derive(Debug)]
#[allow(clippy::upper_case_acronyms)]
pub enum LuaByteCode {
    /// A B: R[A] := R[B]
    Move(Reg, Reg),
    /// A sBx: R[A] := sBx
    LoadI(Reg, i32),
    /// A B: R[A] := K[Bx]
    LoadK(Reg, ConstIdx),

    /// A B C: R[A] := UpValue[B][K[C]:string]
    GetTabUp(Reg, u8, ConstIdx),
    /// A B C: UpValue[A][K[B]:string] := RK(C)
    SetTabUp(Reg, u8, RK),

    /// A B C: R[A] := R[B] + K[C]:number
    AddK(Reg, Reg, ConstIdx),

    /// A B C: R[A] := R[B] + R[C]
    Add(Reg, Reg, Reg),

    /// A B C call C metamethod over R[A] and R[B]
    MMBin(Reg, Reg, ConstIdx),
    /// A sB C k call C metamethod over R[A] and sB
    MMBinI(Reg, i32, ConstIdx),
    /// A B C k call C metamethod over R[A] and K[B]
    MMBinK(Reg, ConstIdx, ConstIdx),

    /// sJ: pc += sJ
    Jmp(i32),
    /// A B k: if ((R[A] == R[B]) ~= k) then pc++
    Eq(Reg, Reg, u8),

    /// A sB k if ((R[A] == sB) ~= k) then pc++
    EQI(Reg, i8, bool),
    /// A sB k: if ((R[A] > sB) ~= k) then pc++
    Gti(Reg, i32, ConstIdx),
    /// A sB k: if ((R[A] >= sB) ~= k) then pc++
    Gei(Reg, u8, u8),

    /// A B C: return R[A], ... ,R[A+B-2]
    Return(u8, u8, u8),

    /// Call k, v: k is callee symbol position, v is argument count, return value not included
    /// A B C: R[A], ... ,R[A+C-2] := R[A](R[A+1], ... ,R[A+B-1])
    Call(Reg, u8, u8),

    /// A (adjust vararg parameters)
    VarArgPrep(u8),
}

impl LuaByteCode {
    fn mnemonic(&self) -> &'static str {
        match self {
            LuaByteCode::Call(..) => "CALL",
            LuaByteCode::GetTabUp(..) => "GETTABUP",
            LuaByteCode::SetTabUp(..) => "SETTABUP",
            LuaByteCode::LoadK(..) => "LOADK",
            LuaByteCode::Move(..) => "MOVE",
            LuaByteCode::LoadI(..) => "LOADI",
            LuaByteCode::Add(..) => "ADD",
            LuaByteCode::AddK(..) => "ADDK",
            LuaByteCode::MMBin(..) => "MMBIN",
            LuaByteCode::MMBinI(..) => "MMBINI",
            LuaByteCode::MMBinK(..) => "MMBINK",
            LuaByteCode::EQI(..) => "EQI",
            LuaByteCode::Gei(..) => "GEI",
            LuaByteCode::Gti(..) => "GTI",
            LuaByteCode::Jmp(..) => "JMP",
            LuaByteCode::Eq(..) => "EQ",
            LuaByteCode::Return(..) => "RETURN",
            LuaByteCode::VarArgPrep(..) => "VARARGPREP",
        }
    }

    fn opcode(&self) -> LuaOpCode {
        match self {
            LuaByteCode::Call(..) => LuaOpCode::OP_CALL,
            LuaByteCode::GetTabUp(..) => LuaOpCode::OP_GETTABUP,
            LuaByteCode::SetTabUp(..) => LuaOpCode::OP_SETTABUP,
            LuaByteCode::LoadK(..) => LuaOpCode::OP_LOADK,
            LuaByteCode::Move(..) => LuaOpCode::OP_MOVE,
            LuaByteCode::LoadI(..) => LuaOpCode::OP_LOADI,
            LuaByteCode::AddK(..) => LuaOpCode::OP_ADDK,
            LuaByteCode::MMBin(..) => LuaOpCode::OP_MMBIN,
            LuaByteCode::MMBinI(..) => LuaOpCode::OP_MMBINI,
            LuaByteCode::MMBinK(..) => LuaOpCode::OP_MMBINK,
            LuaByteCode::Add(..) => LuaOpCode::OP_ADD,
            LuaByteCode::EQI(..) => LuaOpCode::OP_EQI,
            LuaByteCode::Gei(..) => LuaOpCode::OP_GEI,
            LuaByteCode::Gti(..) => LuaOpCode::OP_GTI,
            LuaByteCode::Jmp(..) => LuaOpCode::OP_JMP,
            LuaByteCode::Eq(..) => LuaOpCode::OP_EQ,
            LuaByteCode::Return(..) => LuaOpCode::OP_RETURN,
            LuaByteCode::VarArgPrep(..) => LuaOpCode::OP_VARARGPREP,
        }
    }

    pub fn encode(&self) -> u32 {
        let payload = match *self {
            // ABC
            LuaByteCode::Add(a, b, c) => {
                (c.num() as u32) << 17 | (b.num() as u32) << 9 | a.num() as u32
            }
            // A B k
            LuaByteCode::Eq(a, b, k) => (k as u32) << 17 | (b.num() as u32) << 9 | a.num() as u32,
            // A sB8 K(flag)
            LuaByteCode::EQI(a, sb8, k) => {
                (excess_k!(sb8, 8)) << 9 | a.num() as u32 | (k as u32) << 8
            }
            // A sB k
            LuaByteCode::Gti(a, sb, k) => {
                (k as u32) << 17 | excess_sbx!(sb) << 9 | a.num() as u32 | 1u32 << 8
            }
            LuaByteCode::Gei(a, sb, c) | LuaByteCode::Call(a, sb, c) => {
                (c as u32) << 17 | (sb as u32) << 9 | a.num() as u32
            }
            // A B K
            LuaByteCode::GetTabUp(a, upv, k) => {
                (k as u32) << 17 | (upv as u32) << 9 | a.num() as u32
            }
            // RA, KB, KC with k
            LuaByteCode::MMBinK(ra, kb, kc) => {
                (kc as u32) << 17 | (kb as u32) << 9 | ra.num() as u32 | 1u32 << 8
            }
            // RA, sB, KC with k
            LuaByteCode::MMBinI(ra, sb, k) => {
                (k as u32) << 17 | excess_sbx!(sb) << 9 | ra.num() as u32 | 1u32 << 8
            }
            // RA, RB, KC
            LuaByteCode::AddK(ra, rb, k) | LuaByteCode::MMBin(ra, rb, k) => {
                (k as u32) << 17 | (rb.num() as u32) << 9 | ra.num() as u32
            }
            // A B RK
            LuaByteCode::SetTabUp(a, upv, rk) => {
                let c = match rk {
                    RK::R(Reg::R(r)) => (r as u32) << 17,
                    RK::K(k) => (k as u32) << 17 | 1u32 << 8,
                    _ => unreachable!(),
                };

                c | (upv as u32) << 9 | a.num() as u32
            }
            // A B C all literal
            LuaByteCode::Return(a, b, c) => (c as u32) << 17 | (b as u32) << 9 | a as u32,
            // ABx
            LuaByteCode::LoadK(a, bx) => (bx as u32) << 8 | a.num() as u32,
            // AsBx
            LuaByteCode::LoadI(a, sbx) => excess_sbx!(sbx) << 8 | a.num() as u32,
            // A B
            LuaByteCode::Move(a, b) => (b.num() as u32) << 9 | a.num() as u32,
            // A only
            LuaByteCode::VarArgPrep(a) => (a as u32) << 8,
            // sJ
            LuaByteCode::Jmp(sj) => excess_sj!(sj),
        };

        let op = self.opcode() as u32;
        payload << 7 | op
    }
}

#[derive(Debug)]
pub struct LuaCompiledCode {
    pub byte_codes: Vec<LuaByteCode>,
    pub constants: IndexSet<LuaConstants>,
    pub upvalues: IndexMap<StString, LuaUpValue>,
}

impl LuaCompiledCode {
    pub fn constants_len(&self) -> usize {
        self.constants.len()
    }

    pub fn constants(&self) -> impl Iterator<Item = &LuaConstants> {
        self.constants.as_slice().into_iter()
    }

    pub fn byte_codes(&self) -> &[LuaByteCode] {
        &self.byte_codes
    }

    pub fn disassembly_code(&self, code: &LuaByteCode) -> String {
        let mut s = String::with_capacity(16);
        write!(s, "{:<08X} {:<10} ", code.encode(), code.mnemonic()).unwrap();

        match code {
            // ABC
            LuaByteCode::Add(a, b, c) => {
                write!(s, "R{} R{} R{}", a.num(), b.num(), c.num()).unwrap()
            }
            // A sB8 K(flag)
            LuaByteCode::EQI(a, sb8, k) => write!(s, "R{} {sb8} {}", a.num(), *k as usize).unwrap(),
            // RA, KB, KC with k
            LuaByteCode::MMBinK(ra, kb, kc) => write!(s, "R{} {kb} {kc}", ra.num()).unwrap(),
            // A B k
            LuaByteCode::Eq(a, b, k) => write!(s, "R{} R{} {k}", a.num(), b.num()).unwrap(),
            // A sB k
            LuaByteCode::Gti(a, b, c) => write!(s, "R{} {b} {c}", a.num()).unwrap(),
            LuaByteCode::Gei(a, b, c) | LuaByteCode::Call(a, b, c) => {
                write!(s, "R{} {b} {c}", a.num()).unwrap()
            }
            // RA sB KC with k
            LuaByteCode::MMBinI(ra, sb, kc) => write!(s, "R{} {sb} {kc}", ra.num()).unwrap(),
            // RegA, RegB, K
            LuaByteCode::AddK(ra, rb, k) | LuaByteCode::MMBin(ra, rb, k) => {
                write!(s, "R{} R{} {k}", ra.num(), rb.num()).unwrap()
            }
            // Reg, Upv, K
            LuaByteCode::GetTabUp(reg, upv, k) => write!(s, "R{} {upv} {k}", reg.num()).unwrap(),
            // Reg, Upv, RK
            LuaByteCode::SetTabUp(reg, upv, rk) => {
                write!(s, "R{} {upv} ", reg.num()).unwrap();

                match rk {
                    RK::R(r) => write!(s, "{}", r.num()),
                    RK::K(k) => write!(s, "{}k", k),
                }
                .unwrap();
            }
            // ABC with k
            LuaByteCode::Return(a, b, c) => write!(s, "{a} {b} {c}").unwrap(),
            // ABx
            LuaByteCode::LoadK(a, bx) => write!(s, "R{} {bx}", a.num()).unwrap(),
            // AsBx
            LuaByteCode::LoadI(a, sbx) => write!(s, "R{} {sbx}", a.num()).unwrap(),
            // A B
            LuaByteCode::Move(a, b) => write!(s, "R{} R{}", a.num(), b.num()).unwrap(),
            // A only
            LuaByteCode::VarArgPrep(a) => write!(s, "{a}").unwrap(),
            // sJ
            LuaByteCode::Jmp(sj) => write!(s, "{sj}").unwrap(),
        }

        match code {
            LuaByteCode::MMBin(_, _, k)
            | LuaByteCode::MMBinI(_, _, k)
            | LuaByteCode::MMBinK(_, _, k) => {
                write!(s, " ; {}", self.constants[*k as usize]).unwrap();
            }
            LuaByteCode::LoadK(a, bx) => {
                write!(s, " ; K[{}] = {}", bx, self.constants[*bx as usize]).unwrap();
            }
            LuaByteCode::GetTabUp(a, b, k) => {
                write!(s, " ; _ENV \"{}\"", self.constants[*k as usize]).unwrap()
            }
            LuaByteCode::Call(a, b, c) => {
                if *b == 0 {
                    write!(s, " ; all in ").unwrap();
                } else {
                    write!(s, " ; {} in ", b - 1).unwrap();
                }

                if *c == 0 {
                    write!(s, "all out").unwrap();
                } else {
                    write!(s, "{} out", c - 1).unwrap();
                }
            }
            _ => {}
        }

        s
    }
}

impl Display for LuaCompiledCode {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        // upvalues
        if !self.upvalues.is_empty() {
            writeln!(f, "UpValues:")?;

            for (idx, up_val) in self.upvalues.iter().enumerate() {
                writeln!(f, "{:<4} {:?}", idx, up_val)?
            }
            writeln!(f)?;
        }

        // constants
        if !self.constants.is_empty() {
            writeln!(f, "Constants:")?;
            for (idx, constant) in self.constants.iter().enumerate() {
                writeln!(f, "{:<4} {:?}", idx, constant)?
            }
            writeln!(f)?;
        }

        // ByteCodes
        writeln!(f, "ByteCodes:")?;
        for (idx, bc) in self.byte_codes.iter().enumerate() {
            writeln!(f, "{:<4} {:<20}", idx + 1, self.disassembly_code(bc))?
        }

        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::LuaByteCode;
    use super::Reg;

    #[test]
    fn test_encoding() {
        let code = LuaByteCode::LoadI(Reg::from_raw(0), 2);
        assert_eq!(code.encode(), 0x80008001);
        let code = LuaByteCode::LoadI(Reg::from_raw(3), -65535);
        assert_eq!(code.encode(), 0x00000181);

        let code = LuaByteCode::Return(0, 1, 1);
        assert_eq!(code.encode(), 0x01010046);

        let code = LuaByteCode::Jmp(6);
        assert_eq!(code.encode(), 0x800002B8);
    }
}
