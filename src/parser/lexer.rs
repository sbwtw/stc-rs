use std::iter::Peekable;
use std::str::CharIndices;
use crate::ast::{AstNode, AstVisitor};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerResult = Spanned<Tok, usize, LexicalError>;

#[derive(Debug, Clone)]
pub struct StString(String);

impl From<&str> for StString {
    fn from(s: &str) -> Self {
        Self (s.to_owned())
    }
}

impl From<String> for StString {
    fn from(s: String) -> Self {
        Self (s)
    }
}

impl PartialEq for StString {
    fn eq(&self, other: &Self) -> bool {
        if self.0.len() != other.0.len() {
            return false;
        }

        !self.0.chars().zip(other.0.chars()).any(|(x, y)| x.to_ascii_lowercase() != y.to_ascii_lowercase())
    }
}

impl Eq for StString {}

impl Hash for StString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.0.to_ascii_lowercase().hash(state)
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
    Literal(LiteralType),
    Comma,
    Semicolon,
    If,
    Then,
    Else,
    ElseIf,
    EndIf,
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
            (i, c) => Some(Err(LexicalError::UnexpectedCharacter(i, c))),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::lexer::StString;

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
}
