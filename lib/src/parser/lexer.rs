use crate::ast::*;
use crate::parser::{Buffer, StringBuffer, Tok};
use smallmap::Map;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};

#[derive(Clone)]
pub struct CaseIgnoredChar(char);

impl PartialEq for CaseIgnoredChar {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}

impl PartialOrd for CaseIgnoredChar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        if self.0.is_ascii_alphabetic() && self.0.is_ascii_alphabetic() {
            let x = self.0.to_lowercase();
            let y = self.0.to_lowercase();

            x.partial_cmp(y)
        } else {
            self.0.partial_cmp(&other.0)
        }
    }
}

impl Eq for CaseIgnoredChar {}

impl Ord for CaseIgnoredChar {
    fn cmp(&self, other: &Self) -> Ordering {
        todo!()
    }
}

pub struct Token {
    pub tok: Tok,
    pub start_pos: usize,
    pub end_pos: usize,
}

impl Token {
    pub fn new(tok: Tok, start_pos: usize, end_pos: usize) -> Self {
        Self {
            tok,
            start_pos,
            end_pos,
        }
    }
}

pub(crate) type LexerResult = Result<Token, LexicalError>;

#[derive(Debug, Clone)]
pub enum StString {
    Origin(String),
    Converted(String, String),
}

impl StString {
    pub fn new<S: AsRef<str>>(str: S) -> Self {
        let origin = str.as_ref().to_owned();

        if str.as_ref().as_bytes().iter().any(u8::is_ascii_lowercase) {
            let converted = origin.to_ascii_uppercase();
            Self::Converted(origin, converted)
        } else {
            Self::Origin(origin)
        }
    }

    pub fn empty() -> Self {
        Self::Origin(String::new())
    }

    pub fn is_empty(&self) -> bool {
        self.origin_string().is_empty()
    }

    pub fn origin_string(&self) -> &String {
        match &self {
            Self::Origin(s) => s,
            Self::Converted(origin, _) => origin,
        }
    }

    pub fn string(&self) -> &String {
        match &self {
            Self::Origin(s) => s,
            Self::Converted(_, converted) => converted,
        }
    }

    pub fn len(&self) -> usize {
        match &self {
            Self::Origin(s) => s.len(),
            Self::Converted(orig, _) => orig.len(),
        }
    }

    pub fn chars(&self) -> impl Iterator<Item = CaseIgnoredChar> + '_ {
        self.string().chars().map(CaseIgnoredChar)
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
        self.string().eq(other.string())
    }
}

impl Eq for StString {}

impl PartialEq<str> for StString {
    fn eq(&self, other: &str) -> bool {
        if self.len() != other.len() {
            return false;
        }

        return self.string().eq_ignore_ascii_case(other);
    }
}

impl Hash for StString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.string().hash(state)
    }
}

impl PartialOrd for StString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        self.string().partial_cmp(other.string())
    }
}

impl Ord for StString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}

impl Display for StString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.origin_string())
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BitValue {
    Zero,
    One,
}

#[derive(Debug, Clone, Eq, PartialEq)]
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
    Real(String),
    LReal(String),
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
            LiteralValue::LReal(_) => Box::new(LRealType::new()),
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
            LiteralValue::LReal(x) => write!(f, "{}#{}", Tok::LReal, x),
            LiteralValue::String(s) => write!(f, "{}#{}", Tok::String, s),
        }
    }
}

#[derive(Debug, Clone, PartialOrd, PartialEq, Ord, Eq)]
pub enum LexicalError {
    UnexpectedCharacter(usize, char),
    UnexpectedEnd,
}

#[allow(unused)]
pub struct StLexerOptions {
    allow_unicode_identifier: bool,
    allow_multiple_underline: bool,
    allow_suffix_underline: bool,
    keep_whitespace_token: bool,
}

impl Default for StLexerOptions {
    fn default() -> Self {
        Self {
            allow_unicode_identifier: true,
            allow_multiple_underline: false,
            allow_suffix_underline: false,
            keep_whitespace_token: false,
        }
    }
}

impl StLexerOptions {
    #[allow(unused)]
    pub fn allow_unicode_identifier(mut self, unicode: bool) -> Self {
        self.allow_unicode_identifier = unicode;

        self
    }

    #[allow(unused)]
    pub fn allow_multiple_underline(mut self, multiple: bool) -> Self {
        self.allow_multiple_underline = multiple;

        self
    }

    #[allow(unused)]
    pub fn allow_suffix_underline(mut self, suffix: bool) -> Self {
        self.allow_suffix_underline = suffix;

        self
    }
}

pub struct StLexer<'a> {
    buffer: Box<dyn Buffer + 'a>,
    keywords: Map<StString, Tok>,
    options: StLexerOptions,
}

macro_rules! keywords {
    ($($tok:expr),*) => {{
        let mut keywords = Map::new();
        $(
            let s: StString = $tok.into();
            keywords.insert(s.clone(), $tok);
        )*

        keywords
    }};
}

pub struct StLexerBuilder {
    options: StLexerOptions,
    keywords: Map<StString, Tok>,
}

impl StLexerBuilder {
    pub fn new() -> Self {
        Self {
            options: StLexerOptions::default(),
            keywords: Map::new(),
        }
        .init()
    }

    pub fn from_options(opt: StLexerOptions) -> Self {
        Self {
            options: opt,
            keywords: Map::new(),
        }
        .init()
    }

    fn init(mut self) -> Self {
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
            Tok::VarInput,
            Tok::VarInOut,
            Tok::VarOutput,
            Tok::VarGlobal,
            Tok::EndVar,
            Tok::Retain,
            Tok::Persistent,
            Tok::Type,
            Tok::EndType,
            Tok::Int,
            Tok::Bool,
            Tok::Byte,
            Tok::Real,
            Tok::LReal
        ];

        self.keywords = keywords;
        self
    }

    pub fn build(self, input: &str) -> StLexer {
        StLexer {
            buffer: Box::new(StringBuffer::new(input)),
            keywords: self.keywords,
            options: self.options,
        }
    }
}

impl<'input> StLexer<'input> {
    fn parse_number(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut s = String::from(ch);
        let start_with_zero = ch == '0';

        if start_with_zero {
            let tok = Tok::Literal(LiteralValue::Bit(BitValue::Zero));
            return Some(Ok(Token::new(tok, start, start + 1)));
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

                    let tok = Tok::Literal(LiteralValue::UInt(s.parse().unwrap()));
                    return Some(Ok(Token::new(tok, start, end)));
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

                    let tok = Tok::Literal(LiteralValue::Real(s.parse().unwrap()));
                    return Some(Ok(Token::new(tok, start, end)));
                }
            }
        }
    }

    fn parse_string(&mut self, start: usize) -> Option<LexerResult> {
        let mut escape = false;
        let mut s = String::new();

        loop {
            if escape {
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
                    let tok = Tok::Literal(LiteralValue::String(s));
                    return Some(Ok(Token::new(tok, start, n + 1)));
                }
                (_, Some(c)) => s.push(c),
                _ => return Some(Err(LexicalError::UnexpectedEnd)),
            }
        }
    }

    fn parse_identifier(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut str = String::from(ch);
        let mut underline = ch == '_';

        loop {
            let next = self.buffer.next();
            match next {
                (i, Some('_')) => {
                    if underline && !self.options.allow_multiple_underline {
                        return Some(Err(LexicalError::UnexpectedCharacter(i, '_')));
                    }

                    underline = true;
                }
                _ => underline = false,
            }

            match next {
                (_, Some(c)) if self.is_valid_identifier_character(c) => {
                    str.push(c);
                }
                (n, x) => {
                    self.buffer.stage((n, x));

                    let tok = self.keywords_or_identifier(str);
                    return Some(Ok(Token::new(tok, start, n)));
                }
            }
        }
    }

    fn parse_whitespace(&mut self, start: usize) -> LexerResult {
        loop {
            match self.buffer.next() {
                (_, Some(' ')) | (_, Some('\n')) | (_, Some('\t')) | (_, Some('\r')) => {}
                (i, x) => {
                    self.buffer.stage((i, x));
                    return Ok(Token::new(Tok::Whitespace, start, i));
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
            Some(Ok(Token::new(tok, start, start + 1)))
        } else {
            Some(Ok(Token::new(tok, start, start + 2)))
        }
    }

    fn keywords_or_identifier(&mut self, s: String) -> Tok {
        let st_str = s.into();

        if let Some(keyword) = self.keywords.get(&st_str) {
            return keyword.clone();
        }

        Tok::Identifier(st_str)
    }

    fn is_valid_identifier_character(&self, ch: char) -> bool {
        if self.options.allow_unicode_identifier {
            ch.is_alphabetic() || ch.is_ascii_alphanumeric() || matches!(ch, '_')
        } else {
            ch.is_ascii_alphabetic() || ch.is_ascii_alphanumeric() || matches!(ch, '_')
        }
    }

    fn is_valid_identifier_first_character(&self, ch: char) -> bool {
        if self.options.allow_unicode_identifier {
            ch.is_alphabetic() || matches!(ch, '_')
        } else {
            ch.is_ascii_alphabetic() || matches!(ch, '_')
        }
    }

    fn next_raw(&mut self) -> Option<<Self as Iterator>::Item> {
        match self.buffer.next() {
            (i, Some(' ')) | (i, Some('\t')) | (i, Some('\n')) | (i, Some('\r')) => {
                Some(self.parse_whitespace(i))
            }
            (i, Some('.')) => Some(Ok(Token::new(Tok::Access, i, i + 1))),
            (i, Some('+')) => Some(Ok(Token::new(Tok::Plus, i, i + 1))),
            (i, Some('-')) => Some(Ok(Token::new(Tok::Minus, i, i + 1))),
            (i, Some('/')) => Some(Ok(Token::new(Tok::Division, i, i + 1))),
            (i, Some('(')) => Some(Ok(Token::new(Tok::LeftParentheses, i, i + 1))),
            (i, Some(')')) => Some(Ok(Token::new(Tok::RightParentheses, i, i + 1))),
            (i, Some(',')) => Some(Ok(Token::new(Tok::Comma, i, i + 1))),
            (i, Some(';')) => Some(Ok(Token::new(Tok::Semicolon, i, i + 1))),
            (i, Some('&')) => Some(Ok(Token::new(Tok::BitAnd, i, i + 1))),
            // (i, Some('|')) => Some(Ok((i, Tok::BitOr, i + 1))),
            (i, Some('\"')) => self.parse_string(i),
            (i, Some(c @ '<'))
            | (i, Some(c @ ':'))
            | (i, Some(c @ '>'))
            | (i, Some(c @ '='))
            | (i, Some(c @ '*')) => self.parse_second_char(i, c),
            (start, Some(c)) if c.is_ascii_digit() => self.parse_number(start, c),
            (start, Some(c)) if self.is_valid_identifier_first_character(c) => {
                self.parse_identifier(start, c)
            }
            (i, Some(c)) => Some(Err(LexicalError::UnexpectedCharacter(i, c))),
            (_, None) => None,
        }
    }
}

impl Iterator for StLexer<'_> {
    type Item = LexerResult;

    fn next(&mut self) -> Option<Self::Item> {
        if self.options.keep_whitespace_token {
            return self.next_raw();
        }

        loop {
            match self.next_raw() {
                Some(Ok(tok)) if matches!(tok.tok, Tok::Whitespace) => {}
                x => return x,
            }
        }
    }
}

#[cfg(test)]
mod test {
    use crate::parser::*;

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
        let mut lexer = StLexerBuilder::new().build(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 0);
        assert_eq!(x.end_pos, 2);
        assert_eq!(x.tok, Tok::If);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 3);
        assert_eq!(x.end_pos, 6);
        assert!(matches!(x.tok, Tok::Identifier(_)));

        assert!(matches!(lexer.next(), None));
    }

    #[test]
    fn test_identifier_semicolon() {
        let s = "1 + a;";
        let mut lexer = StLexerBuilder::new().build(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 0);
        assert_eq!(x.end_pos, 1);
        assert!(matches!(x.tok, Tok::Literal(_)));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 2);
        assert_eq!(x.end_pos, 3);
        assert!(matches!(x.tok, Tok::Plus));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 4);
        assert_eq!(x.end_pos, 5);
        assert!(matches!(x.tok, Tok::Identifier(_)));
    }

    #[test]
    fn test_unicode_identifier() {
        let s = "中文 + 中文_1;";
        let mut lexer = StLexerBuilder::new().build(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.start_pos, 0);
        assert_eq!(x.end_pos, 6);
        assert!(matches!(x.tok, Tok::Identifier(_)));
    }

    #[test]
    fn test_multiple_underline() {
        let s = "a__b";
        let mut lexer = StLexerBuilder::new().build(s);

        assert!(matches!(lexer.next(), Some(Err(_))));

        let s = "a__b";
        let lexer_opt = StLexerOptions::default().allow_multiple_underline(true);
        lexer = StLexerBuilder::from_options(lexer_opt).build(s);

        assert!(matches!(lexer.next(), Some(Ok(_))));
    }
}
