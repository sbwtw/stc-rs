use crate::parser::TokenKind;
use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, Copy, Hash)]
pub enum Operator {
    Plus,
    Minus,
    Not,
    Multiply,
    Division,
    BitOr,
    BitAnd,
    Mod,
    Power,
    Xor,

    Less,
    LessEqual,
    Equal,
    NotEqual,
    Greater,
    GreaterEqual,
}

impl Operator {
    pub fn is_unary_operator(&self) -> bool {
        matches!(self, Self::Minus | Self::Not)
    }

    pub fn is_binary_operator(&self) -> bool {
        matches!(
            self,
            Self::LessEqual
                | Self::Less
                | Self::Greater
                | Self::GreaterEqual
                | Self::Equal
                | Self::Plus
                | Self::Minus
                | Self::Division
                | Self::Multiply
                | Self::BitOr
                | Self::BitAnd
                | Self::Mod
                | Self::Power
                | Self::Xor
        )
    }
}

impl From<TokenKind> for Operator {
    fn from(value: TokenKind) -> Self {
        match value {
            TokenKind::Plus => Operator::Plus,
            TokenKind::Minus => Operator::Minus,
            TokenKind::Not => Operator::Not,
            TokenKind::Multiply => Operator::Multiply,
            TokenKind::Division => Operator::Division,
            TokenKind::BitOr => Operator::BitOr,
            TokenKind::BitAnd => Operator::BitAnd,
            TokenKind::Mod => Operator::Mod,
            TokenKind::Power => Operator::Power,
            TokenKind::Xor => Operator::Xor,
            TokenKind::Less => Operator::Less,
            TokenKind::LessEqual => Operator::LessEqual,
            TokenKind::Equal => Operator::Equal,
            TokenKind::NotEqual => Operator::NotEqual,
            TokenKind::Greater => Operator::Greater,
            TokenKind::GreaterEqual => Operator::GreaterEqual,
            _ => unreachable!(),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Operator::Plus => f.write_fmt(format_args!("{}", TokenKind::Plus)),
            Operator::Minus => f.write_fmt(format_args!("{}", TokenKind::Minus)),
            Operator::Not => f.write_fmt(format_args!("{}", TokenKind::Not)),
            Operator::Multiply => f.write_fmt(format_args!("{}", TokenKind::Multiply)),
            Operator::Division => f.write_fmt(format_args!("{}", TokenKind::Division)),
            Operator::BitOr => f.write_fmt(format_args!("{}", TokenKind::BitOr)),
            Operator::BitAnd => f.write_fmt(format_args!("{}", TokenKind::BitAnd)),
            Operator::Mod => f.write_fmt(format_args!("{}", TokenKind::Mod)),
            Operator::Power => f.write_fmt(format_args!("{}", TokenKind::Power)),
            Operator::Xor => f.write_fmt(format_args!("{}", TokenKind::Xor)),
            Operator::Less => f.write_fmt(format_args!("{}", TokenKind::Less)),
            Operator::LessEqual => f.write_fmt(format_args!("{}", TokenKind::LessEqual)),
            Operator::Equal => f.write_fmt(format_args!("{}", TokenKind::Equal)),
            Operator::NotEqual => f.write_fmt(format_args!("{}", TokenKind::NotEqual)),
            Operator::Greater => f.write_fmt(format_args!("{}", TokenKind::Greater)),
            Operator::GreaterEqual => f.write_fmt(format_args!("{}", TokenKind::GreaterEqual)),
        }
    }
}
