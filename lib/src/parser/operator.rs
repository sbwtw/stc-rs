use crate::parser::Tok;
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

impl From<Tok> for Operator {
    fn from(value: Tok) -> Self {
        match value {
            Tok::Plus => Operator::Plus,
            Tok::Minus => Operator::Minus,
            Tok::Not => Operator::Not,
            Tok::Multiply => Operator::Multiply,
            Tok::Division => Operator::Division,
            Tok::BitOr => Operator::BitOr,
            Tok::BitAnd => Operator::BitAnd,
            Tok::Mod => Operator::Mod,
            Tok::Power => Operator::Power,
            Tok::Xor => Operator::Xor,
            Tok::Less => Operator::Less,
            Tok::LessEqual => Operator::LessEqual,
            Tok::Equal => Operator::Equal,
            Tok::NotEqual => Operator::NotEqual,
            Tok::Greater => Operator::Greater,
            Tok::GreaterEqual => Operator::GreaterEqual,
            _ => unreachable!(),
        }
    }
}

impl Display for Operator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match *self {
            Operator::Plus => f.write_fmt(format_args!("{}", Tok::Plus)),
            Operator::Minus => f.write_fmt(format_args!("{}", Tok::Minus)),
            Operator::Not => f.write_fmt(format_args!("{}", Tok::Not)),
            Operator::Multiply => f.write_fmt(format_args!("{}", Tok::Multiply)),
            Operator::Division => f.write_fmt(format_args!("{}", Tok::Division)),
            Operator::BitOr => f.write_fmt(format_args!("{}", Tok::BitOr)),
            Operator::BitAnd => f.write_fmt(format_args!("{}", Tok::BitAnd)),
            Operator::Mod => f.write_fmt(format_args!("{}", Tok::Mod)),
            Operator::Power => f.write_fmt(format_args!("{}", Tok::Power)),
            Operator::Xor => f.write_fmt(format_args!("{}", Tok::Xor)),
            Operator::Less => f.write_fmt(format_args!("{}", Tok::Less)),
            Operator::LessEqual => f.write_fmt(format_args!("{}", Tok::LessEqual)),
            Operator::Equal => f.write_fmt(format_args!("{}", Tok::Equal)),
            Operator::NotEqual => f.write_fmt(format_args!("{}", Tok::NotEqual)),
            Operator::Greater => f.write_fmt(format_args!("{}", Tok::Greater)),
            Operator::GreaterEqual => f.write_fmt(format_args!("{}", Tok::GreaterEqual)),
        }
    }
}
