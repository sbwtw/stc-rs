#[allow(non_snake_case)]
struct LuaABC {
    K: bool,
    A: u8,
    B: u8,
    C: u8,
}

#[allow(non_snake_case)]
struct LuaABx {
    A: u8,
    Bx: u32,
}

#[allow(non_snake_case)]
struct LuaAsBx {
    A: u8,
    sBx: i32,
}

#[allow(non_snake_case)]
struct LuaAx {
    Ax: u32,
}

#[allow(non_snake_case)]
struct LuasJ {
    sJ: i32,
}

#[repr(u8)]
#[allow(non_camel_case_types)]
pub enum LuaOpCode {
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
