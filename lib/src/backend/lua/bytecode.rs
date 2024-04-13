use indexmap::IndexSet;
use std::fmt::{Display, Formatter, Write};
use std::hash::{Hash, Hasher};

macro_rules! excess_k {
    ($v: expr, $k: expr) => {
        ($v as u32).wrapping_add(((2u32.pow($k) - 1) / 2)) & (2u32.pow($k) - 1)
    };
}

macro_rules! excess_sBx {
    ($v: expr) => {
        excess_k!($v, 17)
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
    /// A B: R[A] := R[B]
    Move(u8, u8),
    /// A sBx: R[A] := sBx
    LoadI(u8, i32),
    /// A B: R[A] := K[Bx]
    LoadK(u8, u32),

    /// A B C: R[A] := UpValue[B][K[C]:string]
    GetTabUp(u8, u8, u8),
    /// A B C: UpValue[A][K[B]:string] := RK(C)
    SetTabUp(u8, u8, u8),

    /// A B C: R[A] := R[B] + R[C]
    Add(u8, u8, u8),

    /// A B k: if ((R[A] == R[B]) ~= k) then pc++
    Eq(u8, u8, u8),

    /// A sB k: if ((R[A] > sB) ~= k) then pc++
    Gti(u8, u8, u8),
    /// A sB k: if ((R[A] >= sB) ~= k) then pc++
    Gei(u8, u8, u8),

    /// Call k, v: k is callee symbol position, v is argument count, return value not included
    /// A B C: R[A], ... ,R[A+C-2] := R[A](R[A+1], ... ,R[A+B-1])
    Call(u8, u8, u8),
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
            LuaByteCode::Gei(..) => "GEI",
            LuaByteCode::Gti(..) => "GTI",
            LuaByteCode::Eq(..) => "EQ",
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
            LuaByteCode::Add(..) => LuaOpCode::OP_ADD,
            LuaByteCode::Gei(..) => LuaOpCode::OP_GEI,
            LuaByteCode::Gti(..) => LuaOpCode::OP_GTI,
            LuaByteCode::Eq(..) => LuaOpCode::OP_EQ,
        }
    }

    pub fn encode(&self) -> u32 {
        let payload = match *self {
            // ABC
            LuaByteCode::Call(a, b, c)
            | LuaByteCode::GetTabUp(a, b, c)
            | LuaByteCode::SetTabUp(a, b, c)
            | LuaByteCode::Gei(a, b, c)
            | LuaByteCode::Gti(a, b, c)
            | LuaByteCode::Eq(a, b, c)
            | LuaByteCode::Add(a, b, c) => (c as u32) << 16 | (b as u32) << 8 | a as u32,
            // ABx
            LuaByteCode::LoadK(a, bx) => bx << 8 | a as u32,
            // AsBx
            LuaByteCode::LoadI(a, sbx) => excess_sBx!(sbx) << 8 | a as u32,
            // A B
            LuaByteCode::Move(a, b) => todo!(),
        };

        let op = self.opcode() as u32;
        payload << 7 | op
    }
}

#[derive(Debug)]
pub struct LuaCompiledCode {
    pub byte_codes: Vec<LuaByteCode>,
    pub constants: IndexSet<LuaConstants>,
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
            LuaByteCode::Call(a, b, c)
            | LuaByteCode::GetTabUp(a, b, c)
            | LuaByteCode::SetTabUp(a, b, c)
            | LuaByteCode::Gei(a, b, c)
            | LuaByteCode::Gti(a, b, c)
            | LuaByteCode::Eq(a, b, c)
            | LuaByteCode::Add(a, b, c) => {
                write!(s, "{a} {b} {c}").unwrap();
            }
            // ABx
            LuaByteCode::LoadK(a, bx) => {
                write!(s, "{a} {bx}").unwrap();
            }
            // AsBx
            LuaByteCode::LoadI(a, sbx) => {
                write!(s, "{a} {sbx}").unwrap();
            }
            // A B
            LuaByteCode::Move(a, b) => {
                write!(s, "{a} {b}").unwrap();
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

impl Display for LuaCompiledCode {
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

#[cfg(test)]
mod test {
    use super::LuaByteCode;

    #[test]
    fn test_encoding() {
        let code = LuaByteCode::LoadI(0, 2);
        assert_eq!(code.encode(), 0x80008001);

        let code = LuaByteCode::LoadI(3, -65535);
        assert_eq!(code.encode(), 0x00000181);
    }
}
