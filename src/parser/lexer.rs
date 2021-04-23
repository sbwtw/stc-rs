use std::iter::Peekable;
use std::str::CharIndices;
use crate::ast::{AstNode, AstVisitor};

pub type Spanned<Tok, Loc, Error> = Result<(Loc, Tok, Loc), Error>;
pub type LexerResult = Spanned<Tok, usize, LexicalError>;

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
}

#[derive(Debug, Clone)]
pub enum LiteralType {
    I32(i32),
    U64(u64),
    F32(f32),
    String(String),
}

impl AstNode for LiteralType {
    fn accept(&self, visitor: &mut dyn AstVisitor) {
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
}

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices().peekable(),
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
