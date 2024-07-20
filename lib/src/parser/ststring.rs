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

impl From<TokenKind> for StString {
    fn from(value: TokenKind) -> Self {
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

impl Default for StString {
    fn default() -> Self {
        Self::Origin(String::new())
    }
}
