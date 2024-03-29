use crate::parser::*;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Tok {
    None,
    /// ' ' or '\n', etc.
    Whitespace,
    /// '.'
    Access,
    /// '+'
    Plus,
    /// '-'
    Minus,
    /// '*'
    Multiply,
    /// '**'
    Power,
    /// '/'
    Division,
    /// '('
    LeftParentheses,
    /// ')'
    RightParentheses,
    /// ','
    Comma,
    /// ';'
    Semicolon,
    /// ':'
    Colon,
    /// ':='
    Assign,
    /// '=>'
    AssignRight,
    /// 'R='
    AssignReset,
    /// 'S='
    AssignSet,
    /// '='
    Equal,
    /// '<>'
    NotEqual,
    /// '>'
    Greater,
    /// '>='
    GreaterEqual,
    /// '<'
    Less,
    /// '<='
    LessEqual,
    /// '|' or 'OR'
    BitOr,
    /// '&' or 'AND'
    BitAnd,
    /// '^'
    Deref,
    /// 'MOD'
    Mod,
    /// 'NOT'
    Not,
    /// 'XOR'
    Xor,
    /// 'POINTER'
    Pointer,
    /// 'ARRAY'
    Array,
    /// 'OF'
    Of,
    /// 'IF'
    If,
    /// 'THEN'
    Then,
    /// 'ELSE'
    Else,
    /// 'ELSEIF'
    ElseIf,
    /// 'END_IF'
    EndIf,
    /// 'TO'
    To,
    /// 'FUNCTION'
    Function,
    /// 'END_FUNCTION'
    EndFunction,
    /// 'PROGRAM'
    Program,
    /// 'END_PROGRAM'
    EndProgram,
    /// 'FUNCTION_BLOCK'
    FunctionBlock,
    /// 'END_FUNCTION_BLOCK'
    EndFunctionBlock,
    /// 'STRUCT'
    Struct,
    /// 'END_STRUCT'
    EndStruct,
    /// 'VAR'
    Var,
    /// 'VAR_GLOBAL'
    VarGlobal,
    /// 'VAR_INPUT'
    VarInput,
    /// 'VAR_INOUT'
    VarInOut,
    /// 'VAR_OUTPUT'
    VarOutput,
    /// 'VAR_TEMP'
    VarTemp,
    /// 'VAR_STAT'
    VarStat,
    /// 'END_VAR'
    EndVar,
    /// 'RETAIN'
    Retain,
    /// 'PERSISTENT'
    Persistent,
    /// 'TYPE'
    Type,
    /// 'END_TYPE'
    EndType,
    /// 'BIT', one bit type
    Bit,
    /// 'BOOL', boolean type
    Bool,
    /// 'SINT', 8 bits signed
    SInt,
    /// 'BYTE', 8 bits unsigned
    Byte,
    /// 'INT', 16 bits signed
    Int,
    /// 'UINT', 16 bits unsigned
    UInt,
    /// 'DINT', 32 bits signed
    DInt,
    /// 'UDINT', 32bits unsigned
    UDInt,
    /// 'LINT', 64 bits signed
    LInt,
    /// 'ULINT', 64 bits unsigned
    ULInt,
    /// 'REAL', 32 bits signed
    Real,
    /// 'LREAL', 64 bits unsigned
    LReal,
    /// 'TIME' 32 bits time
    Time,
    /// 'LTIME' 64 bits time
    LTime,
    /// 'STRING', string type
    String,
    /// Literal
    Literal(LiteralValue),
    /// Identifier
    Identifier(StString),
}

impl Tok {
    pub fn is_type(&self) -> bool {
        matches!(self, Tok::Int | Tok::Bool)
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            Tok::Less
                | Tok::LessEqual
                | Tok::Greater
                | Tok::GreaterEqual
                | Tok::Equal
                | Tok::NotEqual
                | Tok::Plus
                | Tok::Minus
                | Tok::Division
                | Tok::Multiply
                | Tok::BitOr
                | Tok::BitAnd
                | Tok::Mod
                | Tok::Power
                | Tok::Not
                | Tok::Xor
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, Tok::Literal(_))
    }
}

impl From<&Tok> for String {
    fn from(value: &Tok) -> Self {
        let tmp_string;

        let s = match value {
            Tok::None => "!!!NONE!!!",
            Tok::Whitespace => " ",
            Tok::Access => ".",
            Tok::Plus => "+",
            Tok::Minus => "-",
            Tok::Multiply => "*",
            Tok::Power => "**",
            Tok::Division => "/",
            Tok::LeftParentheses => "(",
            Tok::RightParentheses => ")",
            Tok::Comma => ",",
            Tok::Semicolon => ";",
            Tok::Colon => ":",
            Tok::Assign => ":=",
            Tok::AssignRight => "=>",
            Tok::AssignSet => "S=",
            Tok::AssignReset => "R=",
            Tok::Equal => "=",
            Tok::NotEqual => "<>",
            Tok::Greater => ">",
            Tok::GreaterEqual => ">=",
            Tok::Less => "<",
            Tok::LessEqual => "<=",
            Tok::BitOr => "OR",
            Tok::BitAnd => "AND",
            Tok::Deref => "^",
            Tok::Mod => "MOD",
            Tok::Xor => "XOR",
            Tok::Not => "NOT",
            Tok::Pointer => "POINTER",
            Tok::Array => "ARRAY",
            Tok::Of => "OF",
            Tok::To => "TO",
            Tok::If => "IF",
            Tok::Then => "THEN",
            Tok::Else => "ELSE",
            Tok::ElseIf => "ELSEIF",
            Tok::EndIf => "END_IF",
            Tok::Function => "FUNCTION",
            Tok::EndFunction => "END_FUNCTION",
            Tok::Program => "PROGRAM",
            Tok::EndProgram => "END_PROGRAM",
            Tok::FunctionBlock => "FUNCTION_BLOCK",
            Tok::EndFunctionBlock => "END_FUNCTION_BLOCK",
            Tok::Struct => "STRUCT",
            Tok::EndStruct => "END_STRUCT",
            Tok::VarGlobal => "VAR_GLOBAL",
            Tok::Var => "VAR",
            Tok::VarInput => "VAR_INPUT",
            Tok::VarInOut => "VAR_INOUT",
            Tok::VarOutput => "VAR_OUTPUT",
            Tok::VarTemp => "VAR_TEMP",
            Tok::VarStat => "VAR_STAT",
            Tok::EndVar => "END_VAR",
            Tok::Retain => "RETAIN",
            Tok::Persistent => "PERSISTENT",
            Tok::Type => "TYPE",
            Tok::EndType => "END_TYPE",
            Tok::Int => "INT",
            Tok::Real => "REAL",
            Tok::LReal => "LREAL",
            Tok::Bit => "BIT",
            Tok::Bool => "BOOL",
            Tok::SInt => "SINT",
            Tok::Byte => "BYTE",
            Tok::UInt => "UINT",
            Tok::DInt => "DINT",
            Tok::UDInt => "UDINT",
            Tok::LInt => "LINT",
            Tok::ULInt => "ULINT",
            Tok::Time => "TIME",
            Tok::LTime => "LTIME",
            Tok::String => "STRING",
            Tok::Literal(x) => {
                tmp_string = format!("{}", x);
                tmp_string.as_str()
            }
            Tok::Identifier(s) => s.origin_string(),
        };

        s.to_owned()
    }
}

impl Display for Tok {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<String>::into(self))
    }
}
