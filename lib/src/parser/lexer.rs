use crate::ast::*;
use crate::parser::token::{Location, Token};
use crate::parser::{Buffer, IterBuffer, StreamBuffer, TokenKind};
use crate::prelude::StString;
use bitflags::bitflags;
use smallmap::Map;
use std::fmt::{self, Display, Formatter};
use std::io;

/// The location info for input
#[derive(Default)]
pub struct LocInfo {
    pub mark: usize,
    pub text_line: usize,
    pub buffer_offset: usize,
}

pub(crate) type LexerResult = Result<Token, LexicalError>;

bitflags! {
    #[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
    pub struct NumberStringFlags: u32 {
        const NONE              = 0b0000_0000_0000_0000;
        const NEGATIVE          = 0b0000_0000_0000_0001;
        const FLOAT             = 0b0000_0000_0000_0010;
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
    loc_info: Vec<LocInfo>,
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
            TokenKind::For,
            TokenKind::EndFor,
            TokenKind::By,
            TokenKind::Do,
            TokenKind::Continue,
            TokenKind::Break,
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
            TokenKind::LReal,
            TokenKind::SInt,
            TokenKind::UInt,
            TokenKind::USInt,
            TokenKind::Array,
            TokenKind::Adr,
            TokenKind::SizeOf
        ];

        self.keywords = keywords;
        self
    }

    pub fn build_str(self, input: &str) -> StLexer {
        StLexer {
            buffer: Box::new(IterBuffer::new(input.chars())),
            keywords: self.keywords,
            options: self.options,
            loc_info: vec![LocInfo::default()],
        }
    }

    pub fn build_iter<'str, T: Iterator<Item = char> + 'str>(self, iter: T) -> StLexer<'str> {
        StLexer {
            buffer: Box::new(IterBuffer::new(iter)),
            keywords: self.keywords,
            options: self.options,
            loc_info: vec![LocInfo::default()],
        }
    }

    pub fn build_file(self, file: &str) -> io::Result<StLexer> {
        Ok(StLexer {
            buffer: Box::new(StreamBuffer::from_file(file)?),
            keywords: self.keywords,
            options: self.options,
            loc_info: vec![LocInfo::default()],
        })
    }
}

impl<'input> StLexer<'input> {
    #[inline]
    pub fn eof(&mut self) -> bool {
        loop {
            match self.buffer.peek1() {
                Some(' ') | Some('\r') | Some('\n') => self.buffer.consume1(),
                None => return true,
                _ => return false,
            }
        }
    }

    #[inline]
    pub fn buffer_offset_by_line(&self, line: usize) -> Option<usize> {
        self.loc_info
            .iter()
            .find(|x| x.text_line == line)
            .map(|x| x.buffer_offset)
    }

    fn record_line_location(&mut self) {
        let text_line = self.buffer.current_line();
        let buffer_offset = self.buffer.buffer_offset();

        self.loc_info.push(LocInfo {
            mark: text_line,
            text_line,
            buffer_offset,
        });
    }

    // ^123.456
    fn parse_number_string(
        &mut self,
        ch: char,
    ) -> Result<(String, NumberStringFlags), LexicalError> {
        self.buffer.consume1();

        let mut flags = NumberStringFlags::NONE;
        let mut s = String::with_capacity(80);
        s.push(ch);
        let start_with_zero = ch == '0';

        // 0 without '.', must be BIT#0
        if start_with_zero && self.buffer.peek1() != Some('.') {
            return Ok((s, flags));
        }

        loop {
            match self.buffer.peek1() {
                Some(c) if c.is_ascii_digit() => {
                    self.buffer.consume1();
                    s.push(c);
                }
                Some('.') => {
                    flags |= NumberStringFlags::FLOAT;
                    s = self.parse_float_string(s)?;
                    break;
                }
                _ => {
                    break;
                }
            }
        }

        Ok((s, flags))
    }

    /// 123^.456, return ateed string
    fn parse_float_string(&mut self, mut s: String) -> Result<String, LexicalError> {
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
                    return Ok(s);
                }
            }
        }
    }

    // parsing a number without annotation prefix
    fn parse_number_no_annotation(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
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
                    return self.parse_floating_before_dot(tok, s);
                }
                _ => {
                    tok.length = s.len();
                    tok.kind = TokenKind::Literal(LiteralValue::UInt(s.parse().unwrap()));
                    return Some(Ok(tok));
                }
            }
        }
    }

    fn parse_floating_before_dot(&mut self, mut tok: Token, mut s: String) -> Option<LexerResult> {
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
                            self.buffer.line_offset(),
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

    /// Identifier, Keywords or literal with type annotation prefix
    fn parse_words(&mut self, mut tok: Token, ch: char) -> Option<LexerResult> {
        let mut str = String::with_capacity(32);
        str.push(ch);

        // if ch == '_' && !self.options.allow_multiple_underline{
        //     if let Some(c) = self.buffer.peek1() {
        //         if self.is_valid_identifier_character(c) {
        //             return
        //         }
        //     }
        // }

        loop {
            let next = self.buffer.peek1();
            if let Some('_') = next {
                // if current is first underline, but next character is also underline, can't eat
                if !self.options.allow_multiple_underline && self.buffer.peek(2) == Some('_') {
                    break;
                }
            }

            match next {
                Some(c) if self.is_valid_identifier_character(c) => {
                    self.buffer.consume1();
                    str.push(c);
                }
                x => break,
            }
        }

        tok.length = str.len();
        tok.kind = self.keywords_or_identifier(str);
        if self.buffer.peek1() != Some('#') || !tok.kind.is_type() {
            return Some(Ok(tok));
        }

        // current token is type annotation prefix, like: sint#123
        self.buffer.consume1();
        self.parse_annotated_literal(tok)
    }

    // parsing a literal with annotation prefix
    fn parse_annotated_literal(&mut self, tok: Token) -> Option<LexerResult> {
        let annotation_length = tok.length + 1; // +1 for '#'
        let ch = self.buffer.peek1()?;

        // TODO: ensure annotation is matching result
        let mut number = match self.parse_number_no_annotation(tok, ch) {
            None => return None,
            Some(Err(e)) => return Some(Err(e)),
            Some(Ok(v)) => v,
        };
        number.length += annotation_length;

        Some(Ok(number))
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
                    self.record_line_location();
                    return Ok(tok);
                }
            }
        }
    }

    // 2 or more characters operator
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

    // literal with type annotation prefix, like: int#123 or lreal#1.234e1
    fn type_annotation_literal(&mut self) -> TokenKind {
        todo!()
    }

    #[inline]
    fn is_valid_identifier_character(&self, ch: char) -> bool {
        if self.options.allow_unicode_identifier {
            ch.is_alphabetic() || ch.is_ascii_alphanumeric() || matches!(ch, '_')
        } else {
            ch.is_ascii_alphabetic() || ch.is_ascii_alphanumeric() || matches!(ch, '_')
        }
    }

    #[inline]
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
            location: Location {
                mark: self.buffer.current_line(),
                offset: self.buffer.line_offset(),
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
            Some(c) if c.is_ascii_digit() => self.parse_number_no_annotation(tok, c),
            Some(c) if self.is_valid_identifier_first_character(c) => {
                self.buffer.consume1();
                self.parse_words(tok, c)
            }
            Some(c) => {
                self.buffer.consume1();
                Some(Err(LexicalError::UnexpectedCharacter(
                    self.buffer.current_line(),
                    self.buffer.line_offset(),
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
        assert_eq!(x.location.offset, 0);
        assert_eq!(x.location.mark, 0);
        assert_eq!(x.length, 2);
        assert_eq!(x.kind, TokenKind::If);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.location.offset, 3);
        assert_eq!(x.length, 3);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));

        assert!(lexer.next().is_none());
    }

    #[test]
    fn test_identifier_semicolon() {
        let s = "1 + \na;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.location.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Literal(_)));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.location.offset, 2);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Plus));

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.location.mark, 1);
        assert_eq!(x.location.offset, 0);
        assert_eq!(x.length, 1);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn test_unicode_identifier() {
        let s = "中文 + 中文_1;";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert_eq!(x.location.offset, 0);
        assert_eq!(x.length, 6);
        assert!(matches!(x.kind, TokenKind::Identifier(_)));
    }

    #[test]
    fn test_multiple_underline() {
        let s = "a_b";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let c = lexer.next().unwrap().unwrap();
        assert!(matches!(c.kind, TokenKind::Identifier(_)));
        assert!(matches!(c.length, 3));

        let s = "a__b";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let c = lexer.next().unwrap().unwrap();
        assert!(matches!(c.kind, TokenKind::Identifier(_)));
        assert!(matches!(c.length, 1));

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
    fn test_array() {
        let s = "array [ 1..2] of bit";
        let mut lexer = StLexerBuilder::new().build_iter(s.chars());

        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Array));
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

    #[test]
    fn test_multiline() {
        let s = "a\r\nb";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Identifier(..)));
        let y = lexer.next().unwrap().unwrap();
        assert!(matches!(y.kind, TokenKind::Identifier(..)));
        assert_eq!(y.location.mark, 1);
        assert_eq!(y.location.offset, 0);

        let s = "a\n\rb";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Identifier(..)));
        let y = lexer.next().unwrap().unwrap();
        assert!(matches!(y.kind, TokenKind::Identifier(..)));
        assert_eq!(y.location.mark, 1);
        assert_eq!(y.location.offset, 0);

        let s = "a\n\nb";
        let mut lexer = StLexerBuilder::new().build_str(s);

        let x = lexer.next().unwrap().unwrap();
        assert!(matches!(x.kind, TokenKind::Identifier(..)));
        let y = lexer.next().unwrap().unwrap();
        assert!(matches!(y.kind, TokenKind::Identifier(..)));
        assert_eq!(y.location.mark, 2);
        assert_eq!(y.location.offset, 0);
    }

    #[test]
    fn test_literal() {
        macro_rules! test_literal_parse {
            ($str:literal, $except:pat, $len:expr) => {
                let mut lexer = StLexerBuilder::new().build_str($str);

                let x = lexer.next().unwrap().unwrap();
                assert!(
                    matches!(x.kind, $except),
                    "kind mismatch, result is {}",
                    x.kind
                );
                assert_eq!(x.length, $len);
            };
        }

        test_literal_parse!("sint#123", TokenKind::Literal(..), 8);
        // test_literal_parse!("sint#123", TokenKind::Literal(LiteralValue::SInt(..)), 8);
        test_literal_parse!("uint#123", TokenKind::Literal(LiteralValue::UInt(123)), 8);
        // test_literal_parse!("sint#-123", TokenKind::Literal(..), 9);
        test_literal_parse!("0.5", TokenKind::Literal(LiteralValue::LReal(..)), 3);
        // test_literal_parse!("-0.5", TokenKind::Literal(LiteralValue::LReal(..)), 4);
    }

    #[test]
    fn test_line_info() {
        macro_rules! test_line_lexer {
            ($str:literal) => {{
                let mut lexer = StLexerBuilder::new().build_str($str);
                while let Some(_) = lexer.next() {}

                lexer
            }};
        }

        let lexer = test_line_lexer!("a\nb\nc");
        assert_eq!(lexer.buffer_offset_by_line(0), Some(0));
        assert_eq!(lexer.buffer_offset_by_line(1), Some(2));
        assert_eq!(lexer.buffer_offset_by_line(2), Some(4));
        assert_eq!(lexer.buffer_offset_by_line(3), None);

        let lexer = test_line_lexer!("a\r\nb\r\nc");
        assert_eq!(lexer.buffer_offset_by_line(0), Some(0));
        assert_eq!(lexer.buffer_offset_by_line(1), Some(3));
        assert_eq!(lexer.buffer_offset_by_line(2), Some(6));
        assert_eq!(lexer.buffer_offset_by_line(3), None);
    }
}
