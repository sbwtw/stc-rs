use crate::parser::*;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub enum Tok {
    /// '.'
    Access,
    /// '+'
    Plus,
    /// '-'
    Minus,
    /// '*'
    Multiply,
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
    /// '^'
    Deref,
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
    /// 'VAR'
    Var,
    /// 'VAR_GLOBAL'
    VarGlobal,
    /// 'END_VAR'
    EndVar,
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
    /// 'LINT', 64 bits signed
    LInt,
    /// 'ULINT', 64 bits unsigned
    ULInt,
    /// Literal
    Literal(LiteralType),
    /// Identifier
    Identifier(StString),
}

impl Into<String> for &Tok {
    fn into(self) -> String {
        let s = match &self {
            Tok::Access => ".",
            Tok::Plus => "+",
            Tok::Minus => "-",
            Tok::Multiply => "*",
            Tok::Division => "/",
            Tok::LeftParentheses => "(",
            Tok::RightParentheses => ")",
            Tok::Comma => ",",
            Tok::Semicolon => ";",
            Tok::Colon => ":",
            Tok::Assign => ":=",
            Tok::If => "IF",
            Tok::Then => "THEN",
            Tok::Else => "ELSE",
            Tok::ElseIf => "ELSEIF",
            Tok::EndIf => "END_IF",
            Tok::Function => "FUNCTION",
            Tok::EndFunction => "END_FUNCTION",
            Tok::VarGlobal => "VAR_GLOBAL",
            Tok::Var => "VAR",
            Tok::EndVar => "END_VAR",
            Tok::Int => "INT",
            Tok::Identifier(s) => &s.origin_string(),
            _ => unimplemented!(),
        };

        s.to_owned()
    }
}

impl Into<StString> for Tok {
    fn into(self) -> StString {
        let s: String = (&self).into();

        s.into()
    }
}

impl Display for Tok {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", Into::<String>::into(self))
    }
}
