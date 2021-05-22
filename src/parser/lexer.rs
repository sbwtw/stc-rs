use crate::ast::*;
use crate::parser::Tok;
use std::any::Any;
use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::rc::Rc;
use std::str::CharIndices;

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerResult = Spanned<Tok, usize, LexicalError>;

#[derive(Debug, Clone)]
pub struct StString {
    origin_string: Rc<String>,
    converted_string: Rc<String>,
}

impl StString {
    pub fn new<S: AsRef<str>>(str: S) -> Self {
        Self {
            origin_string: Rc::new(str.as_ref().to_owned()),
            converted_string: Rc::new(str.as_ref().to_ascii_uppercase().to_owned()),
        }
    }

    pub fn origin_string(&self) -> &String {
        &self.origin_string
    }
}

impl From<&str> for StString {
    fn from(s: &str) -> Self {
        Self::new(s)
    }
}

impl From<String> for StString {
    fn from(s: String) -> Self {
        Self::new(s)
    }
}

impl PartialEq for StString {
    fn eq(&self, other: &Self) -> bool {
        self.converted_string.eq(&other.converted_string)
    }
}

impl Eq for StString {}

impl PartialEq<str> for StString {
    fn eq(&self, other: &str) -> bool {
        other.to_ascii_uppercase().eq(&*self.converted_string)
    }
}

impl Hash for StString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.converted_string.hash(state)
    }
}

#[derive(Debug, Clone)]
pub enum BitValue {
    Zero,
    One,
}

#[derive(Debug, Clone)]
pub enum LiteralValue {
    Bit(BitValue),
    Bool(bool),
    Byte(u8),
    SInt(i8),
    Int(i16),
    UInt(u16),
    DInt(i32),
    UDInt(u32),
    LInt(i64),
    ULInt(u64),
    Real(f32),
    String(String),
}

impl LiteralValue {
    pub fn ty(&self) -> Box<dyn Type> {
        match self {
            LiteralValue::Bit(_) => Box::new(BitType::new()),
            LiteralValue::Bool(_) => Box::new(BoolType::new()),
            LiteralValue::Byte(_) => Box::new(ByteType::new()),
            LiteralValue::SInt(_) => Box::new(SIntType::new()),
            LiteralValue::Int(_) => Box::new(IntType::new()),
            LiteralValue::UInt(_) => Box::new(UIntType::new()),
            LiteralValue::DInt(_) => Box::new(DIntType::new()),
            LiteralValue::UDInt(_) => Box::new(UDIntType::new()),
            LiteralValue::LInt(_) => Box::new(LIntType::new()),
            LiteralValue::ULInt(_) => Box::new(ULIntType::new()),
            LiteralValue::Real(_) => Box::new(RealType::new()),
            LiteralValue::String(_) => Box::new(StringType::new()),
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Bit(BitValue::Zero) => write!(f, "{}#{}", Tok::Bit, 0),
            LiteralValue::Bit(BitValue::One) => write!(f, "{}#{}", Tok::Bit, 1),
            LiteralValue::Bool(x) => write!(f, "{}#{}", Tok::Bool, x),
            LiteralValue::Int(x) => write!(f, "{}#{}", Tok::Int, x),
            LiteralValue::UInt(x) => write!(f, "{}#{}", Tok::UInt, x),
            LiteralValue::Byte(x) => write!(f, "{}#{}", Tok::Byte, x),
            LiteralValue::SInt(x) => write!(f, "{}#{}", Tok::SInt, x),
            LiteralValue::DInt(x) => write!(f, "{}#{}", Tok::DInt, x),
            LiteralValue::UDInt(x) => write!(f, "{}#{}", Tok::UDInt, x),
            LiteralValue::LInt(x) => write!(f, "{}#{}", Tok::LInt, x),
            LiteralValue::ULInt(x) => write!(f, "{}#{}", Tok::ULInt, x),
            LiteralValue::Real(x) => write!(f, "{}#{}", Tok::Real, x),
            LiteralValue::String(s) => write!(f, "{}#{}", Tok::String, s),
        }
    }
}

impl AstNode for LiteralValue {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn accept(&self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(self)
    }

    fn accept_mut(&mut self, visitor: &mut dyn AstVisitorMut) {
        visitor.visit_literal_mut(self)
    }
}

#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(usize, char),
    UnexpectedEnd,
}

struct LexerBuffer<'input> {
    chars: CharIndices<'input>,
    len: usize,
    stage: Option<(usize, Option<char>)>,
}

impl<'input> LexerBuffer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices(),
            len: input.len(),
            stage: None,
        }
    }

    pub fn next(&mut self) -> (usize, Option<char>) {
        if let Some(r) = self.stage.take() {
            return r;
        }

        match self.chars.next() {
            Some((index, ch)) => (index, Some(ch)),
            _ => (self.len, None),
        }
    }

    pub fn stage(&mut self, stage: (usize, Option<char>)) {
        self.stage = Some(stage);
    }
}

pub struct Lexer<'input> {
    buffer: LexerBuffer<'input>,
    keywords: HashMap<StString, Tok>,
}

macro_rules! keywords {
    ($($tok:expr),*) => {{
        let mut keywords = HashMap::new();
        $(
            keywords.insert($tok.into(), $tok);
        )*

        keywords
    }};
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let keywords = keywords![
            Tok::BitAnd,
            Tok::BitOr,
            Tok::Xor,
            Tok::Mod,
            Tok::Not,
            Tok::If,
            Tok::Else,
            Tok::Then,
            Tok::ElseIf,
            Tok::EndIf,
            Tok::Function,
            Tok::EndFunction,
            Tok::Program,
            Tok::EndProgram,
            Tok::Struct,
            Tok::EndStruct,
            Tok::Var,
            Tok::VarGlobal,
            Tok::EndVar,
            Tok::Retain,
            Tok::Persistent,
            Tok::Type,
            Tok::EndType,
            Tok::Int,
            Tok::Bool
        ];

        Self {
            buffer: LexerBuffer::new(input),
            keywords,
        }
    }

    fn parse_number(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut s = String::from(ch);
        let start_with_zero = ch == '0';

        if start_with_zero {
            return Some(Ok((
                start,
                Tok::Literal(LiteralValue::Bit(BitValue::Zero)),
                start + 1,
            )));
        }

        loop {
            match self.buffer.next() {
                (_, Some(c)) if c.is_ascii_digit() => {
                    s.push(c);
                }
                (n, Some('.')) => {
                    self.buffer.stage((n, Some('.')));

                    return self.parse_floating(start, s);
                }
                (end, x) => {
                    self.buffer.stage((end, x));

                    return Some(Ok((
                        start,
                        Tok::Literal(LiteralValue::UInt(s.parse().unwrap())),
                        end,
                    )));
                }
            }
        }
    }

    fn parse_floating(&mut self, start: usize, mut s: String) -> Option<LexerResult> {
        let mut dot = false;

        loop {
            match self.buffer.next() {
                (_, Some('.')) if !dot => {
                    dot = true;
                    s.push('.');
                }
                (_, Some(c)) if c.is_ascii_digit() => {
                    s.push(c);
                }
                (end, x) => {
                    self.buffer.stage((end, x));

                    return Some(Ok((
                        start,
                        Tok::Literal(LiteralValue::Real(s.parse().unwrap())),
                        end,
                    )));
                }
            }
        }
    }

    fn parse_string(&mut self, start: usize) -> Option<LexerResult> {
        let mut escape = false;
        let mut s = String::new();

        loop {
            if escape == true {
                escape = false;

                match self.buffer.next() {
                    (_, Some('\"')) => s.push('\"'),
                    (n, Some(c)) => return Some(Err(LexicalError::UnexpectedCharacter(n, c))),
                    _ => return Some(Err(LexicalError::UnexpectedEnd)),
                }
            }

            match self.buffer.next() {
                (_, Some('\\')) => escape = true,
                (n, Some('\"')) => {
                    return Some(Ok((start, Tok::Literal(LiteralValue::String(s)), n + 1)))
                }
                (_, Some(c)) => s.push(c),
                _ => return Some(Err(LexicalError::UnexpectedEnd)),
            }
        }
    }

    fn parse_identifier(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut str = String::from(ch);

        loop {
            match self.buffer.next() {
                (_, Some(c))
                    if c.is_ascii_alphabetic() || c.is_ascii_alphanumeric() || c == '_' =>
                {
                    str.push(c);
                }
                (n, x) => {
                    self.buffer.stage((n, x));

                    let tok = self.keywords_or_identifier(str);
                    return Some(Ok((start, tok, n)));
                }
            }
        }
    }

    fn parse_second_char(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let (tok, stage) = match (ch, self.buffer.next()) {
            ('<', (_, Some('='))) => (Tok::LessEqual, None),
            ('<', (_, Some('>'))) => (Tok::NotEqual, None),
            ('<', x) => (Tok::Less, Some(x)),

            ('>', (_, Some('='))) => (Tok::GreaterEqual, None),
            ('>', x) => (Tok::Greater, Some(x)),

            (':', (_, Some('='))) => (Tok::Assign, None),
            (':', x) => (Tok::Colon, Some(x)),

            ('*', (_, Some('*'))) => (Tok::Power, None),
            ('*', x) => (Tok::Multiply, Some(x)),

            ('=', (_, Some('>'))) => (Tok::AssignRight, None),
            ('=', x) => (Tok::Equal, Some(x)),

            _ => unreachable!(),
        };

        // one char eaten, stage second state
        if let Some(s) = stage {
            self.buffer.stage(s);
            Some(Ok((start, tok, start + 1)))
        } else {
            Some(Ok((start, tok, start + 2)))
        }
    }

    fn keywords_or_identifier(&mut self, s: String) -> Tok {
        let st_str = s.into();
        if let Some(keyword) = self.keywords.get(&st_str) {
            return keyword.clone();
        }

        return Tok::Identifier(st_str);
    }
}

impl<'input> Iterator for Lexer<'input> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        match self.buffer.next() {
            (_, Some(' ')) | (_, Some('\t')) | (_, Some('\n')) | (_, Some('\r')) => self.next(),
            (i, Some('.')) => Some(Ok((i, Tok::Access, i + 1))),
            (i, Some('+')) => Some(Ok((i, Tok::Plus, i + 1))),
            (i, Some('-')) => Some(Ok((i, Tok::Minus, i + 1))),
            (i, Some('/')) => Some(Ok((i, Tok::Division, i + 1))),
            (i, Some('(')) => Some(Ok((i, Tok::LeftParentheses, i + 1))),
            (i, Some(')')) => Some(Ok((i, Tok::RightParentheses, i + 1))),
            (i, Some(',')) => Some(Ok((i, Tok::Comma, i + 1))),
            (i, Some(';')) => Some(Ok((i, Tok::Semicolon, i + 1))),
            // (i, Some('|')) => Some(Ok((i, Tok::BitOr, i + 1))),
            (i, Some('&')) => Some(Ok((i, Tok::BitAnd, i + 1))),
            (i, Some('\"')) => self.parse_string(i),
            (i, Some(c @ '<'))
            | (i, Some(c @ ':'))
            | (i, Some(c @ '>'))
            | (i, Some(c @ '='))
            | (i, Some(c @ '*')) => self.parse_second_char(i, c),
            (start, Some(c)) if c.is_ascii_digit() => self.parse_number(start, c),
            (start, Some(c)) if c.is_ascii_alphabetic() || c == '_' => {
                self.parse_identifier(start, c)
            }
            (i, Some(c)) => Some(Err(LexicalError::UnexpectedCharacter(i, c))),
            (_, None) => None,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::{BitValue, Lexer, LiteralValue, StString, Tok};

    #[test]
    fn test_st_string() {
        let s1: StString = "abc".into();
        let s2: StString = String::from("AbC").into();
        assert_eq!(s1, s2);

        let s1: StString = "abc".into();
        let s2: StString = String::from("Abd").into();
        assert_ne!(s1, s2);

        let s1: StString = "_test中文".into();
        let s2: StString = "_tEST中文".into();
        assert_eq!(s1, s2);
    }

    #[test]
    fn test_st_keywords() {
        let s = "if abc";
        let mut lexer = Lexer::new(s);

        assert!(matches!(lexer.next(), Some(Ok((0, Tok::If, 2)))));
        assert!(matches!(lexer.next(), Some(Ok((3, Tok::Identifier(_), 6)))));
        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn test_identifier_semicolon() {
        let s = "1 + a;";
        let mut lexer = Lexer::new(s);

        assert!(matches!(lexer.next(), Some(Ok((0, Tok::Literal(_), 1)))));
        assert!(matches!(lexer.next(), Some(Ok((2, Tok::Plus, 3)))));
        assert!(matches!(lexer.next(), Some(Ok((4, Tok::Identifier(_), 5)))));
        assert!(matches!(lexer.next(), Some(Ok((5, Tok::Semicolon, 6)))));
    }

    #[test]
    fn test_zero_number() {
        let s = "a + 0;";
        let mut lexer = Lexer::new(s);

        assert!(matches!(lexer.next(), Some(Ok((0, Tok::Identifier(_), 1)))));
        assert!(matches!(lexer.next(), Some(Ok((2, Tok::Plus, 3)))));
        assert!(matches!(
            lexer.next(),
            Some(Ok((4, Tok::Literal(LiteralValue::Bit(BitValue::Zero)), 5)))
        ));
        assert!(matches!(lexer.next(), Some(Ok((5, Tok::Semicolon, 6)))));
    }
}
