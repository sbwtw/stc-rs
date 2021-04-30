use std::iter::Peekable;
use std::str::CharIndices;
use crate::ast::{AstNode, AstVisitor};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerResult = Spanned<Tok, usize, LexicalError>;

#[derive(Debug, Clone)]
pub struct StString {
    origin_string: String,
    converted_string: String,
}

impl StString {
    pub fn new<S: AsRef<str>>(str: S) -> Self {
        Self {
            origin_string: str.as_ref().to_owned(),
            converted_string: str.as_ref().to_ascii_uppercase().to_owned(),
        }
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

impl Hash for StString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.converted_string.hash(state)
    }
}

#[derive(Debug, Clone)]
pub enum Tok {
    Plus,
    Minus,
    Multiply,
    Division,
    LeftParentheses,
    RightParentheses,
    Comma,
    Semicolon,
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
    Literal(LiteralType),
    Identifier(StString),
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    I32(i32),
    U64(u64),
    F32(f32),
    String(String),
}

impl AstNode for LiteralType {
    fn accept(&mut self, visitor: &mut dyn AstVisitor) {
        visitor.visit_literal(self)
    }
}

#[derive(Debug)]
pub enum LexicalError {
    UnexpectedCharacter(usize, char),
    UnexpectedEnd,
}

pub struct Lexer<'input> {
    chars: Peekable<CharIndices<'input>>,
    keywords: HashMap<StString, Tok>,
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("IF".into(), Tok::If);
        keywords.insert("ELSE".into(), Tok::Else);
        keywords.insert("THEN".into(), Tok::Then);
        keywords.insert("ELSEIF".into(), Tok::ElseIf);
        keywords.insert("ENDIF".into(), Tok::EndIf);

        Self {
            chars: input.char_indices().peekable(),
            keywords,
        }
    }

    fn parse_integer(
        &mut self,
        start: usize,
        ch: char,
    ) -> Option<LexerResult> {
        let mut s = String::from(ch);
        let mut end = start;

        loop {
            match self.chars.peek() {
                Some((n, c)) if c.is_ascii_digit() => {
                    s.push(*c);
                    end = *n;
                    self.chars.next();
                }
                Some((_, '.')) => return self.parse_floating(start, s),
                _ => {
                    return Some(Ok((
                        start,
                        Tok::Literal(LiteralType::U64(s.parse().unwrap())),
                        end + 1,
                    )))
                }
            }
        }
    }

    fn parse_floating(
        &mut self,
        start: usize,
        mut s: String,
    ) -> Option<LexerResult> {
        let mut dot = false;
        let mut end = start;

        loop {
            match self.chars.peek() {
                Some((n, '.')) if !dot => {
                    dot = true;

                    s.push('.');
                    end = *n;
                    self.chars.next();
                }
                Some((n, c)) if c.is_ascii_digit() => {
                    s.push(*c);
                    end = *n;
                    self.chars.next();
                }
                _ => {
                    return Some(Ok((
                        start,
                        Tok::Literal(LiteralType::F32(s.parse().unwrap())),
                        end + 1,
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

                match self.chars.next()? {
                    (_, '\"') => s.push('\"'),
                    (n, c) => return Some(Err(LexicalError::UnexpectedCharacter(n, c))),
                }
            }

            match self.chars.next()? {
                (_, '\\') => escape = true,
                (n, '\"') => return Some(Ok((start, Tok::Literal(LiteralType::String(s)), n + 1))),
                (_, c) => s.push(c),
            }
        }
    }

    fn parse_identifier(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut str = String::from(ch);
        let mut last_end = start;

        loop {
            match self.chars.peek() {
                Some((end, c)) if c.is_ascii_alphabetic() || *c == '_' => {
                    str.push(c.clone());
                    last_end = *end;
                    self.chars.next();
                }
                Some((end, _)) => {
                    let end = *end;
                    let tok = self.keywords_or_identifier(str);
                    return Some(Ok((start, tok, end)));
                }
                None => {
                    let tok = self.keywords_or_identifier(str);
                    return Some(Ok((start, tok, last_end + 1)));
                }
            }
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
        match self.chars.next()? {
            (_, ' ') | (_, '\t') | (_, '\n') | (_, '\r') => self.next(),
            (i, '+') => Some(Ok((i, Tok::Plus, i + 1))),
            (i, '-') => Some(Ok((i, Tok::Minus, i + 1))),
            (i, '*') => Some(Ok((i, Tok::Multiply, i + 1))),
            (i, '/') => Some(Ok((i, Tok::Division, i + 1))),
            (i, '(') => Some(Ok((i, Tok::LeftParentheses, i + 1))),
            (i, ')') => Some(Ok((i, Tok::RightParentheses, i + 1))),
            (i, ',') => Some(Ok((i, Tok::Comma, i + 1))),
            (i, ';') => Some(Ok((i, Tok::Semicolon, i + 1))),
            (i, '\"') => self.parse_string(i),
            (start, c) if c.is_ascii_digit() && c != '0' => self.parse_integer(start, c),
            (start, c) if c.is_ascii_alphabetic() || c == '_' => self.parse_identifier(start, c),
            (i, c) => Some(Err(LexicalError::UnexpectedCharacter(i, c))),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::StString;
    use crate::parser::{Lexer, Tok, LexerResult};

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
        let ident: StString = "abc".into();
        assert!(matches!(lexer.next(), Some(Ok((3, Tok::Identifier(ident), 6)))));
        assert!(matches!(lexer.next(), None));
    }
}
