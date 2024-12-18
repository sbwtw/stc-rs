use std::cmp::Ordering;
use std::fmt;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};

use super::TokenKind;

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

#[derive(Debug, Clone, Default)]
pub struct StString(String);

impl StString {
    pub fn new<S: AsRef<str>>(str: S) -> Self {
        let origin = str.as_ref().to_owned();
        Self(origin)
    }

    pub fn empty() -> Self {
        Self(String::new())
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn string(&self) -> &String {
        &self.0
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub fn chars(&self) -> impl Iterator<Item = StChar> + '_ {
        self.string().chars().map(StChar)
    }
}

impl From<TokenKind> for StString {
    fn from(value: TokenKind) -> Self {
        StString(Into::<String>::into(&value))
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

impl PartialEq for StString {
    fn eq(&self, other: &Self) -> bool {
        self.0.eq_ignore_ascii_case(&other.0)
    }
}

impl Eq for StString {}

impl PartialEq<str> for StString {
    fn eq(&self, other: &str) -> bool {
        if self.len() != other.len() {
            return false;
        }

        self.string().eq_ignore_ascii_case(other)
    }
}

impl Hash for StString {
    fn hash<H: Hasher>(&self, state: &mut H) {
        for c in self.0.to_ascii_uppercase().as_bytes() {
            state.write_u8(*c);
        }
    }
}

impl PartialOrd for StString {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for StString {
    fn cmp(&self, other: &Self) -> Ordering {
        self.0.to_uppercase().cmp(&other.0.to_uppercase())
    }
}

impl Display for StString {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        f.write_str(self.string())
    }
}

impl AsRef<str> for StString {
    fn as_ref(&self) -> &str {
        self.string().as_str()
    }
}

#[cfg(test)]
mod tests {
    use crate::prelude::*;

    #[test]
    fn test_ststring() {
        let s1: StString = "abc".into();
        let s2: StString = "ABC".into();

        assert_eq!(s1, s2);
    }
}
