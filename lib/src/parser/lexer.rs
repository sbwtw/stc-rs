use crate::ast::*;
use crate::parser::token::{Token, TokenPosition};
use crate::parser::{Buffer, StreamBuffer, StringBuffer, TokenKind};
use crate::prelude::StString;
use smallmap::Map;
use std::fmt::{self, Display, Formatter};
use std::io;

pub(crate) type LexerResult = Result<Token, LexicalError>;

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
    pub fn ty(&self) -> Type {
        match self {
            LiteralValue::Bit(_) => BitType::new_type(),
            LiteralValue::Bool(_) => BoolType::new_type(),
            LiteralValue::Byte(_) => ByteType::new_type(),
            LiteralValue::SInt(_) => SIntType::new_type(),
            LiteralValue::Int(_) => IntType::new_type(),
            LiteralValue::UInt(_) => UIntType::new_type(),
            LiteralValue::DInt(_) => DIntType::new_type(),
            LiteralValue::UDInt(_) => UDIntType::new_type(),
            LiteralValue::LInt(_) => LIntType::new_type(),
            LiteralValue::ULInt(_) => ULIntType::new_type(),
            LiteralValue::Real(_) => RealType::new_type(),
            LiteralValue::LReal(_) => LRealType::new_type(),
            LiteralValue::String(_) => StringType::new_type(),
        }
    }
}

impl Display for LiteralValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            LiteralValue::Bit(BitValue::Zero) => write!(f, "{}#{}", TokenKind::Bit, 0),
            LiteralValue::Bit(BitValue::One) => write!(f, "{}#{}", TokenKind::Bit, 1),
            LiteralValue::Bool(x) => write!(f, "{}#{}", TokenKind::Bool, x),
            LiteralValue::Int(x) => write!(f, "{}#{}", TokenKind::Int, x),
            LiteralValue::UInt(x) => write!(f, "{}#{}", TokenKind::UInt, x),
            LiteralValue::Byte(x) => write!(f, "{}#{}", TokenKind::Byte, x),
            LiteralValue::SInt(x) => write!(f, "{}#{}", TokenKind::SInt, x),
            LiteralValue::DInt(x) => write!(f, "{}#{}", TokenKind::DInt, x),
            LiteralValue::UDInt(x) => write!(f, "{}#{}", TokenKind::UDInt, x),
            LiteralValue::LInt(x) => write!(f, "{}#{}", TokenKind::LInt, x),
            LiteralValue::ULInt(x) => write!(f, "{}#{}", TokenKind::ULInt, x),
            LiteralValue::Real(x) => write!(f, "{}#{}", TokenKind::Real, x),
            LiteralValue::LReal(x) => write!(f, "{}#{}", TokenKind::LReal, x),
            LiteralValue::String(s) => write!(f, "{}#{}", TokenKind::String, s),
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
    keywords: Map<StString, TokenKind>,
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
    keywords: Map<StString, TokenKind>,
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
            TokenKind::BitAnd,
            TokenKind::BitOr,
            TokenKind::Xor,
            TokenKind::Mod,
            TokenKind::Not,
            TokenKind::If,
            TokenKind::Of,
            TokenKind::Else,
            TokenKind::Then,
            TokenKind::ElseIf,
            TokenKind::EndIf,
            TokenKind::Function,
            TokenKind::EndFunction,
            TokenKind::Program,
            TokenKind::EndProgram,
            TokenKind::Struct,
            TokenKind::EndStruct,
            TokenKind::Var,
            TokenKind::VarInput,
            TokenKind::VarInOut,
            TokenKind::VarOutput,
            TokenKind::VarGlobal,
            TokenKind::VarTemp,
            TokenKind::EndVar,
            TokenKind::Retain,
            TokenKind::Persistent,
            TokenKind::Type,
            TokenKind::EndType,
            TokenKind::Bit,
            TokenKind::Int,
            TokenKind::Bool,
            TokenKind::Byte,
            TokenKind::Real,
            TokenKind::LReal
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
    pub fn eof(&mut self) -> bool {
        loop {
            match self.buffer.peek1() {
                Some(' ') | Some('\r') | Some('\n') => self.buffer.consume1(),
                None => return true,
                _ => return false,
            }
        }
    }

    fn parse_number(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
        self.buffer.consume1();

        let mut s = String::from(ch);
        let start_with_zero = ch == '0';

        if start_with_zero && self.buffer.peek1() != Some('.') {
            tok.kind = TokenKind::Literal(LiteralValue::Bit(BitValue::Zero));
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
                    tok.kind = TokenKind::Literal(LiteralValue::UInt(s.parse().unwrap()));
                    return Some(Ok(tok));
                }
            }
        }
    }

    /// 123^.456
    fn parse_floating(&mut self, mut tok: Token, mut s: String) -> Option<LexerResult> {
        let mut dot = false;

        loop {
            match self.buffer.peek1() {
                // avoid range like 1..3
                Some('.') if !dot && self.buffer.peek(2) != Some('.') => {
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
                    tok.kind = TokenKind::Literal(LiteralValue::LReal(s));
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
                    tok.kind = TokenKind::Literal(LiteralValue::String(s));
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
                    tok.kind = self.keywords_or_identifier(str);
                    return Some(Ok(tok));
                }
            }
        }
    }

    fn parse_whitespace(&mut self, mut tok: Token) -> LexerResult {
        tok.kind = TokenKind::Whitespace;

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
            ('<', Some('=')) => tok.kind = TokenKind::LessEqual,
            ('<', Some('>')) => tok.kind = TokenKind::NotEqual,
            ('<', _) => {
                tok.kind = TokenKind::Less;
                tok.length = 1;
            }

            ('>', Some('=')) => tok.kind = TokenKind::GreaterEqual,
            ('>', _) => {
                tok.kind = TokenKind::Greater;
                tok.length = 1;
            }

            (':', Some('=')) => tok.kind = TokenKind::Assign,
            (':', _) => {
                tok.kind = TokenKind::Colon;
                tok.length = 1;
            }

            ('*', Some('*')) => tok.kind = TokenKind::Power,
            ('*', _) => {
                tok.kind = TokenKind::Multiply;
                tok.length = 1;
            }

            ('=', Some('>')) => tok.kind = TokenKind::AssignRight,
            ('=', _) => {
                tok.kind = TokenKind::Equal;
                tok.length = 1;
            }

            _ => unreachable!(),
        };

        if tok.length == 2 {
            self.buffer.consume1()
        }

        Some(Ok(tok))
    }

    fn keywords_or_identifier(&mut self, s: String) -> TokenKind {
        let st_str = s.into();

        if let Some(keyword) = self.keywords.get(&st_str) {
            return keyword.clone();
        }

        TokenKind::Identifier(st_str)
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
                mark: self.buffer.current_line(),
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
                match self.buffer.peek1() {
                    Some('.') => {
                        self.buffer.consume1();
                        tok.kind = TokenKind::DotRange;
                        Some(Ok(tok))
                    }
                    _ => {
                        tok.kind = TokenKind::DotAccess;
                        Some(Ok(tok))
                    }
                }
            }
            Some('+') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Plus;
                Some(Ok(tok))
            }
            Some('-') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Minus;
                Some(Ok(tok))
            }
            Some('/') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Division;
                Some(Ok(tok))
            }
            Some('(') => {
                self.buffer.consume1();
                tok.kind = TokenKind::LeftParentheses;
                Some(Ok(tok))
            }
            Some(')') => {
                self.buffer.consume1();
                tok.kind = TokenKind::RightParentheses;
                Some(Ok(tok))
            }
            Some('[') => {
                self.buffer.consume1();
                tok.kind = TokenKind::LeftBracket;
                Some(Ok(tok))
            }
            Some(']') => {
                self.buffer.consume1();
                tok.kind = TokenKind::RightBracket;
                Some(Ok(tok))
            }
            Some(',') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Comma;
                Some(Ok(tok))
            }
            Some(';') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Semicolon;
                Some(Ok(tok))
            }
            Some('&') => {
                self.buffer.consume1();
                tok.kind = TokenKind::BitAnd;
                Some(Ok(tok))
            }
            Some('^') => {
                self.buffer.consume1();
                tok.kind = TokenKind::Deref;
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
                Some(Ok(tok)) if matches!(tok.kind, TokenKind::Whitespace) => {}
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
        assert_eq!(x.pos.mark, 0);
        assert_eq!(x.length, 2);
        assert_eq!(x.kind, TokenKind::If);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 3);
        assert_eq!(x.length, 3);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_identifier_semicolon() {
        let s = "1 + \na;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Literal(_)));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 2);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Plus));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.mark, 1);
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn test_unicode_identifier() {
        let s = "中文 + 中文_1;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 6);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));
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
            lexer.next().unwrap().unwrap().kind,
            TokenKind::AssignRight
        ));
    }

    #[test]
    fn test_numbers() {
        let s = "0.123";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.pos.offset, 0);
        assert_eq!(x.length, 5);
        assert!(matches!(x.kind, TokenKind::Literal(LiteralValue::LReal(_))));
    }

    #[test]
    fn test_array() {
        let s = "array[0..1] of bit";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Identifier(..)));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::LeftBracket));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Literal(..)));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::DotRange));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Literal(..)));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::RightBracket));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Of));
        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Bit));
    }
}
