use crate::ast::{AstNode, AstVisitor, AstVisitorMut};
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::str::CharIndices;

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

impl<'input> Lexer<'input> {
    pub fn new(input: &'input str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("IF".into(), Tok::If);
        keywords.insert("ELSE".into(), Tok::Else);
        keywords.insert("THEN".into(), Tok::Then);
        keywords.insert("ELSEIF".into(), Tok::ElseIf);
        keywords.insert("END_IF".into(), Tok::EndIf);

        Self {
            buffer: LexerBuffer::new(input),
            keywords,
        }
    }

    fn parse_integer(&mut self, start: usize, ch: char) -> Option<LexerResult> {
        let mut s = String::from(ch);

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
                        Tok::Literal(LiteralType::U64(s.parse().unwrap())),
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
                        Tok::Literal(LiteralType::F32(s.parse().unwrap())),
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
                    return Some(Ok((start, Tok::Literal(LiteralType::String(s)), n + 1)))
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
                (_, Some(c)) if c.is_ascii_alphabetic() || c == '_' => {
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
            (i, Some('+')) => Some(Ok((i, Tok::Plus, i + 1))),
            (i, Some('-')) => Some(Ok((i, Tok::Minus, i + 1))),
            (i, Some('*')) => Some(Ok((i, Tok::Multiply, i + 1))),
            (i, Some('/')) => Some(Ok((i, Tok::Division, i + 1))),
            (i, Some('(')) => Some(Ok((i, Tok::LeftParentheses, i + 1))),
            (i, Some(')')) => Some(Ok((i, Tok::RightParentheses, i + 1))),
            (i, Some(',')) => Some(Ok((i, Tok::Comma, i + 1))),
            (i, Some(';')) => Some(Ok((i, Tok::Semicolon, i + 1))),
            (i, Some('\"')) => self.parse_string(i),
            (start, Some(c)) if c.is_ascii_digit() && c != '0' => self.parse_integer(start, c),
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
    use crate::parser::{Lexer, StString, Tok};

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
}
