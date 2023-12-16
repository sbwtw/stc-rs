use crate::ast::*;
use crate::parser::{Buffer, StreamBuffer, StringBuffer, Tok};
use smallmap::Map;
use std::cmp::Ordering;
use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::io;

#[derive(Clone)]
pub struct StChar(char);

impl PartialEq for StChar {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}

impl PartialOrd for StChar {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Eq for StChar {}

impl Ord for StChar {
    fn cmp(&self, other: &Self) -> Ordering {
        if self.0.is_ascii_alphabetic() && other.0.is_ascii_alphabetic() {
            let x = self.0.to_lowercase();
            let y = self.0.to_lowercase();

            x.cmp(y)
        } else {
            self.0.cmp(&other.0)
        }
    }
}

pub struct TokenPosition {
    pub line: usize,
    pub offset: usize,
}

pub struct Token {
    pub tok: Tok,
    pub length: usize,
    pub pos: TokenPosition,
}

impl Token {
    pub fn new(tok: Tok, start_pos: usize, end_pos: usize) -> Self {
        Self {
            tok,
            length: 0,
            pos: TokenPosition { line: 0, offset: 0 },
        }
    }
}

impl Default for Token {
    fn default() -> Self {
        Self {
            tok: Tok::None,
            length: 0,
            pos: TokenPosition { line: 0, offset: 0 },
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

    fn string(&self) -> &String {
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

    pub fn chars(&self) -> impl Iterator<Item = StChar> + '_ {
        self.string().chars().map(StChar)
    }
}

impl From<Tok> for StString {
    fn from(value: Tok) -> Self {
        StString::Origin(Into::<String>::into(&value))
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

impl<'a> From<&'a StString> for &'a str {
    fn from(value: &'a StString) -> Self {
        value.string().as_str()
    }
}

impl AsRef<str> for StString {
    fn as_ref(&self) -> &str {
        self.into()
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
        Some(self.cmp(other))
    }
}

impl Ord for StString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.string().cmp(other.string())
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
    UnexpectedCharacter(usize, usize, char),
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

impl Default for StLexerBuilder {
    fn default() -> Self {
        Self {
            options: StLexerOptions::default(),
            keywords: Map::new(),
        }
        .init()
    }
}

impl StLexerBuilder {
    pub fn new() -> Self {
        Default::default()
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
            Tok::VarTemp,
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

    pub fn build_str(self, input: &str) -> StLexer {
        StLexer {
            buffer: Box::new(StringBuffer::new(input)),
            keywords: self.keywords,
            options: self.options,
        }
    }

    pub fn build_file(self, file: &str) -> io::Result<StLexer> {
        Ok(StLexer {
            buffer: Box::new(StreamBuffer::from_file(file)?),
            keywords: self.keywords,
            options: self.options,
        })
    }
}

impl<'input> StLexer<'input> {
    fn parse_number(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
        self.buffer.consume1();

        let mut s = String::from(ch);
        let start_with_zero = ch == '0';

        if start_with_zero && self.buffer.peek1() != Some('.') {
            tok.tok = Tok::Literal(LiteralValue::Bit(BitValue::Zero));
            return Some(Ok(tok));
        }

        loop {
            match self.buffer.peek1() {
                Some(c) if c.is_ascii_digit() => {
                    self.buffer.consume1();
                    s.push(c);
                }
                Some('.') => {
                    return self.parse_floating(tok, s);
                }
                _ => {
                    tok.length = s.len();
                    tok.tok = Tok::Literal(LiteralValue::UInt(s.parse().unwrap()));
                    return Some(Ok(tok));
                }
            }
        }
    }

    fn parse_floating(&mut self, mut tok: Token, mut s: String) -> Option<LexerResult> {
        let mut dot = false;

        loop {
            match self.buffer.peek1() {
                Some('.') if !dot => {
                    self.buffer.consume1();
                    dot = true;
                    s.push('.');
                }
                Some(c) if c.is_ascii_digit() => {
                    self.buffer.consume1();
                    s.push(c);
                }
                _ => {
                    tok.length = s.len();
                    tok.tok = Tok::Literal(LiteralValue::LReal(s));
                    return Some(Ok(tok));
                }
            }
        }
    }

    fn parse_string(&mut self, mut tok: Token) -> Option<LexerResult> {
        let mut escape = false;
        let mut s = String::new();

        loop {
            if escape {
                escape = false;

                match self.buffer.peek1() {
                    Some('\"') => {
                        self.buffer.consume1();
                        s.push('\"')
                    }
                    Some(c) => {
                        return Some(Err(LexicalError::UnexpectedCharacter(
                            self.buffer.current_line(),
                            self.buffer.current_offset(),
                            c,
                        )))
                    }
                    _ => return Some(Err(LexicalError::UnexpectedEnd)),
                }
            }

            match self.buffer.peek1() {
                Some('\\') => {
                    self.buffer.consume1();
                    escape = true;
                }
                Some('\"') => {
                    self.buffer.consume1();
                    tok.tok = Tok::Literal(LiteralValue::String(s));
                    return Some(Ok(tok));
                }
                Some(c) => {
                    self.buffer.consume1();
                    s.push(c)
                }
                _ => return Some(Err(LexicalError::UnexpectedEnd)),
            }
        }
    }

    fn parse_identifier(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
        let mut str = String::from(ch);
        let mut underline = ch == '_';

        loop {
            let next = self.buffer.peek1();
            match next {
                Some('_') => {
                    if underline && !self.options.allow_multiple_underline {
                        return Some(Err(LexicalError::UnexpectedCharacter(
                            self.buffer.current_line(),
                            self.buffer.current_offset(),
                            '_',
                        )));
                    }

                    underline = true;
                }
                _ => underline = false,
            }

            match next {
                Some(c) if self.is_valid_identifier_character(c) => {
                    self.buffer.consume1();
                    str.push(c);
                }
                x => {
                    tok.length = str.len();
                    tok.tok = self.keywords_or_identifier(str);
                    return Some(Ok(tok));
                }
            }
        }
    }

    fn parse_whitespace(&mut self, mut tok: Token) -> LexerResult {
        tok.tok = Tok::Whitespace;

        loop {
            match self.buffer.peek1() {
                Some(' ') | Some('\n') | Some('\t') | Some('\r') => {
                    self.buffer.consume1();
                    tok.length += 1;
                }
                _ => {
                    return Ok(tok);
                }
            }
        }
    }

    fn parse_second_char(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
        tok.length = 2;

        match (ch, self.buffer.peek1()) {
            ('<', Some('=')) => tok.tok = Tok::LessEqual,
            ('<', Some('>')) => tok.tok = Tok::NotEqual,
            ('<', _) => {
                tok.tok = Tok::Less;
                tok.length = 1;
            }

            ('>', Some('=')) => tok.tok = Tok::GreaterEqual,
            ('>', _) => {
                tok.tok = Tok::Greater;
                tok.length = 1;
            }

            (':', Some('=')) => tok.tok = Tok::Assign,
            (':', _) => {
                tok.tok = Tok::Colon;
                tok.length = 1;
            }

            ('*', Some('*')) => tok.tok = Tok::Power,
            ('*', _) => {
                tok.tok = Tok::Multiply;
                tok.length = 1;
            }

            ('=', Some('>')) => tok.tok = Tok::AssignRight,
            ('=', _) => {
                tok.tok = Tok::Equal;
                tok.length = 1;
            }

            _ => unreachable!(),
        };

        if tok.length == 2 {
            self.buffer.consume1()
        }

        Some(Ok(tok))
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
        let mut tok = Token {
            length: 1,
            pos: TokenPosition {
                line: self.buffer.current_line(),
                offset: self.buffer.current_offset(),
            },
            ..Default::default()
        };

        match self.buffer.peek1() {
            Some(' ') | Some('\t') | Some('\n') | Some('\r') => {
                self.buffer.consume1();
                Some(self.parse_whitespace(tok))
            }
            Some('.') => {
                self.buffer.consume1();
                tok.tok = Tok::Access;
                Some(Ok(tok))
            }
            Some('+') => {
                self.buffer.consume1();
                tok.tok = Tok::Plus;
                Some(Ok(tok))
            }
            Some('-') => {
                self.buffer.consume1();
                tok.tok = Tok::Minus;
                Some(Ok(tok))
            }
            Some('/') => {
                self.buffer.consume1();
                tok.tok = Tok::Division;
                Some(Ok(tok))
            }
            Some('(') => {
                self.buffer.consume1();
                tok.tok = Tok::LeftParentheses;
                Some(Ok(tok))
            }
            Some(')') => {
                self.buffer.consume1();
                tok.tok = Tok::RightParentheses;
                Some(Ok(tok))
            }
            Some(',') => {
                self.buffer.consume1();
                tok.tok = Tok::Comma;
                Some(Ok(tok))
            }
            Some(';') => {
                self.buffer.consume1();
                tok.tok = Tok::Semicolon;
                Some(Ok(tok))
            }
            Some('&') => {
                self.buffer.consume1();
                tok.tok = Tok::BitAnd;
                Some(Ok(tok))
            }
            Some('\"') => {
                self.buffer.consume1();
                self.parse_string(tok)
            }
            Some(c @ '<') | Some(c @ ':') | Some(c @ '>') | Some(c @ '=') | Some(c @ '*') => {
                self.buffer.consume1();
                self.parse_second_char(tok, c)
            }
            Some(c) if c.is_ascii_digit() => self.parse_number(tok, c),
            Some(c) if self.is_valid_identifier_first_character(c) => {
                self.buffer.consume1();
                self.parse_identifier(tok, c)
            }
            Some(c) => {
                self.buffer.consume1();
                Some(Err(LexicalError::UnexpectedCharacter(
                    self.buffer.current_line(),
                    self.buffer.current_offset(),
                    c,
                )))
            }
            None => None,
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
    use std::cmp::Ordering;

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
    fn test_st_string_order() {
        let s1: StString = "abc".into();
        let s2: StString = "AbC".into();
        assert_eq!(s1.cmp(&s2), Ordering::Equal);
        assert_eq!(s2.cmp(&s1), Ordering::Equal);

        let s1: StString = "abD".into();
        let s2: StString = "AbC".into();
        assert_eq!(s1.cmp(&s2), Ordering::Greater);
        assert_eq!(s2.cmp(&s1), Ordering::Less);

        let s1: StString = "abc".into();
        let s1_chars = s1.chars();
        let s2: StString = "AbC".into();
        let s2_chars = s2.chars();
        assert!(s1_chars
            .zip(s2_chars)
            .all(|(x, y)| x.cmp(&y) == Ordering::Equal && y.cmp(&x) == Ordering::Equal))
    }

    #[test]
    fn test_st_keywords() {
        let s = "if abc";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.pos.line, 0);
        assert_eq!(x.length, 2);
        assert_eq!(x.tok, Tok::If);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 3);
        assert_eq!(x.length, 3);
        assert!(matches!(x.tok, Tok::Identifier(_)));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_identifier_semicolon() {
        let s = "1 + \na;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.tok, Tok::Literal(_)));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 2);
        assert_eq!(x.length, 1);
        assert!(matches!(x.tok, Tok::Plus));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.line, 1);
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.tok, Tok::Identifier(_)));
    }

    #[test]
    fn test_unicode_identifier() {
        let s = "中文 + 中文_1;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 6);
        assert!(matches!(x.tok, Tok::Identifier(_)));
    }

    #[test]
    fn test_multiple_underline() {
        let s = "a__b";
        let mut lexer = StLexerBuilder::new().build_str(s);

        assert!(matches!(lexer.next(), Some(Err(_))));

        let s = "a__b";
        let lexer_opt = StLexerOptions::default().allow_multiple_underline(true);
        lexer = StLexerBuilder::from_options(lexer_opt).build_str(s);

        assert!(matches!(lexer.next(), Some(Ok(_))));
    }

    #[test]
    fn test_out_assign() {
        let s = "=>";
        let mut lexer = StLexerBuilder::new().build_str(s);

        assert!(matches!(
            lexer.next().unwrap().unwrap().tok,
            Tok::AssignRight
        ));
    }

    #[test]
    fn test_numbers() {
        let s = "0.123";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 5);
        assert!(matches!(x.tok, Tok::Literal(LiteralValue::LReal(_))));
    }
}
