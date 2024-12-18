use crate::parser::*;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone, Default, Copy)]
pub struct Location {
    pub mark: usize,
    pub offset: usize,
}

#[derive(Debug)]
pub struct Token {
    pub kind: TokenKind,
    pub length: usize,
    pub location: Location,
}

impl Token {
    pub fn new(kind: TokenKind, start_pos: usize, end_pos: usize) -> Self {
        Self {
            kind,
            length: 0,
            location: Location { mark: 0, offset: 0 },
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::None,
            length: 0,
            location: Location { mark: 0, offset: 0 },
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum TokenKind {
    None,
    /// ' ' or '\n', etc.
    Whitespace,
    /// '.'
    DotAccess,
    /// '..'
    DotRange,
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
    /// '['
    LeftBracket,
    /// ']'
    RightBracket,
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
    /// 'FOR'
    For,
    /// 'BY'
    By,
    /// 'END_FOR'
    EndFor,
    /// 'CONTINUE'
    Continue,
    /// 'BREAK'
    Break,
    /// 'DO'
    Do,
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
    /// 'SizeOf' Operator
    SizeOf,
    /// 'Adr' Operator
    Adr,
    /// 'BIT', one bit type
    Bit,
    /// 'BOOL', boolean type
    Bool,
    /// 'SINT', 8 bits signed
    SInt,
    /// 'USINT', 8 bits unsigned
    USInt,
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

impl TokenKind {
    pub fn is_type(&self) -> bool {
        matches!(
            self,
            TokenKind::Bit
                | TokenKind::Bool
                | TokenKind::Byte
                | TokenKind::SInt
                | TokenKind::USInt
                | TokenKind::Int
                | TokenKind::UInt
                | TokenKind::Real
                | TokenKind::LReal
                | TokenKind::String
        )
    }

    #[inline]
    pub fn is_keywords(&self) -> bool {
        matches!(
            *self,
            TokenKind::If
                | TokenKind::Else
                | TokenKind::ElseIf
                | TokenKind::EndIf
                | TokenKind::For
                | TokenKind::EndFor
                | TokenKind::By
                | TokenKind::Break
                | TokenKind::Do
                | TokenKind::Continue
                | TokenKind::Program
                | TokenKind::EndProgram
                | TokenKind::Var
                | TokenKind::VarGlobal
                | TokenKind::Then
                | TokenKind::Array
                | TokenKind::EndVar
        )
    }

    pub fn kind_match(&self, rhs: &TokenKind) -> bool {
        match *self {
            TokenKind::AssignRight => matches!(rhs, TokenKind::AssignRight),
            TokenKind::Assign => matches!(rhs, TokenKind::Assign),
            TokenKind::Int => matches!(rhs, TokenKind::Int),
            TokenKind::Byte => matches!(rhs, TokenKind::Byte),
            TokenKind::Function => matches!(rhs, TokenKind::Function),
            TokenKind::EndFunction => matches!(rhs, TokenKind::EndFunction),
            TokenKind::Program => matches!(rhs, TokenKind::Program),
            TokenKind::EndProgram => matches!(rhs, TokenKind::EndProgram),
            TokenKind::Colon => matches!(rhs, TokenKind::Colon),
            TokenKind::Comma => matches!(rhs, TokenKind::Comma),
            TokenKind::Semicolon => matches!(rhs, TokenKind::Semicolon),
            TokenKind::Type => matches!(rhs, TokenKind::Type),
            TokenKind::EndType => matches!(rhs, TokenKind::EndType),
            TokenKind::Var => matches!(rhs, TokenKind::Var),
            TokenKind::VarGlobal => matches!(rhs, TokenKind::VarGlobal),
            TokenKind::EndVar => matches!(rhs, TokenKind::EndVar),
            TokenKind::Struct => matches!(rhs, TokenKind::Struct),
            TokenKind::EndStruct => matches!(rhs, TokenKind::EndStruct),
            TokenKind::LeftParentheses => matches!(rhs, TokenKind::LeftParentheses),
            TokenKind::RightParentheses => matches!(rhs, TokenKind::RightParentheses),
            TokenKind::If => matches!(rhs, TokenKind::If),
            TokenKind::Then => matches!(rhs, TokenKind::Then),
            TokenKind::Else => matches!(rhs, TokenKind::Else),
            TokenKind::ElseIf => matches!(rhs, TokenKind::ElseIf),
            TokenKind::EndIf => matches!(rhs, TokenKind::EndIf),
            TokenKind::Array => matches!(rhs, TokenKind::Array),
            TokenKind::LeftBracket => matches!(rhs, TokenKind::LeftBracket),
            TokenKind::RightBracket => matches!(rhs, TokenKind::RightBracket),
            TokenKind::Of => matches!(rhs, TokenKind::Of),
            TokenKind::DotRange => matches!(rhs, TokenKind::DotRange),
            TokenKind::DotAccess => matches!(rhs, TokenKind::DotAccess),
            TokenKind::Identifier(..) => matches!(rhs, TokenKind::Identifier(..)),
            _ => panic!("Token {self} not handle"),
        }
    }

    pub fn is_operator(&self) -> bool {
        matches!(
            self,
            TokenKind::Less
                | TokenKind::LessEqual
                | TokenKind::Greater
                | TokenKind::GreaterEqual
                | TokenKind::Equal
                | TokenKind::NotEqual
                | TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Division
                | TokenKind::Multiply
                | TokenKind::BitOr
                | TokenKind::BitAnd
                | TokenKind::Mod
                | TokenKind::Power
                | TokenKind::Not
                | TokenKind::Xor
        )
    }

    pub fn is_literal(&self) -> bool {
        matches!(self, TokenKind::Literal(_))
    }
}

impl From<&TokenKind> for String {
    fn from(value: &TokenKind) -> Self {
        let tmp_string;

        let s = match value {
            TokenKind::None => "!!!NONE!!!",
            TokenKind::Whitespace => " ",
            TokenKind::DotAccess => ".",
            TokenKind::DotRange => "..",
            TokenKind::Plus => "+",
            TokenKind::Minus => "-",
            TokenKind::Multiply => "*",
            TokenKind::Power => "**",
            TokenKind::Division => "/",
            TokenKind::LeftParentheses => "(",
            TokenKind::RightParentheses => ")",
            TokenKind::LeftBracket => "[",
            TokenKind::RightBracket => "]",
            TokenKind::Comma => ",",
            TokenKind::Semicolon => ";",
            TokenKind::Colon => ":",
            TokenKind::Assign => ":=",
            TokenKind::AssignRight => "=>",
            TokenKind::AssignSet => "S=",
            TokenKind::AssignReset => "R=",
            TokenKind::Equal => "=",
            TokenKind::NotEqual => "<>",
            TokenKind::Greater => ">",
            TokenKind::GreaterEqual => ">=",
            TokenKind::Less => "<",
            TokenKind::LessEqual => "<=",
            TokenKind::BitOr => "OR",
            TokenKind::BitAnd => "AND",
            TokenKind::Deref => "^",
            TokenKind::Mod => "MOD",
            TokenKind::Xor => "XOR",
            TokenKind::Not => "NOT",
            TokenKind::Pointer => "POINTER",
            TokenKind::Array => "ARRAY",
            TokenKind::Of => "OF",
            TokenKind::To => "TO",
            TokenKind::If => "IF",
            TokenKind::Then => "THEN",
            TokenKind::Else => "ELSE",
            TokenKind::ElseIf => "ELSEIF",
            TokenKind::EndIf => "END_IF",
            TokenKind::Function => "FUNCTION",
            TokenKind::EndFunction => "END_FUNCTION",
            TokenKind::Program => "PROGRAM",
            TokenKind::EndProgram => "END_PROGRAM",
            TokenKind::FunctionBlock => "FUNCTION_BLOCK",
            TokenKind::EndFunctionBlock => "END_FUNCTION_BLOCK",
            TokenKind::Struct => "STRUCT",
            TokenKind::EndStruct => "END_STRUCT",
            TokenKind::VarGlobal => "VAR_GLOBAL",
            TokenKind::Var => "VAR",
            TokenKind::VarInput => "VAR_INPUT",
            TokenKind::VarInOut => "VAR_INOUT",
            TokenKind::VarOutput => "VAR_OUTPUT",
            TokenKind::VarTemp => "VAR_TEMP",
            TokenKind::VarStat => "VAR_STAT",
            TokenKind::EndVar => "END_VAR",
            TokenKind::Retain => "RETAIN",
            TokenKind::Persistent => "PERSISTENT",
            TokenKind::Type => "TYPE",
            TokenKind::EndType => "END_TYPE",
            TokenKind::SizeOf => "SIZEOF",
            TokenKind::Adr => "ADR",
            TokenKind::Int => "INT",
            TokenKind::Real => "REAL",
            TokenKind::LReal => "LREAL",
            TokenKind::Bit => "BIT",
            TokenKind::Bool => "BOOL",
            TokenKind::SInt => "SINT",
            TokenKind::USInt => "USINT",
            TokenKind::Byte => "BYTE",
            TokenKind::UInt => "UINT",
            TokenKind::DInt => "DINT",
            TokenKind::UDInt => "UDINT",
            TokenKind::LInt => "LINT",
            TokenKind::ULInt => "ULINT",
            TokenKind::Time => "TIME",
            TokenKind::LTime => "LTIME",
            TokenKind::String => "STRING",
            TokenKind::For => "FOR",
            TokenKind::By => "BY",
            TokenKind::EndFor => "END_FOR",
            TokenKind::Continue => "CONTINUE",
            TokenKind::Break => "BREAK",
            TokenKind::Do => "DO",
            TokenKind::Literal(x) => {
                tmp_string = format!("{}", x);
                tmp_string.as_str()
            }
            TokenKind::Identifier(s) => s.string(),
        };

        s.to_owned()
    }
}

impl Display for TokenKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<String>::into(self))
    }
}
