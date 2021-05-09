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
