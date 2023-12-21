use strum_macros::{Display, EnumIter};
use tower_lsp::lsp_types::{SemanticTokenModifier, SemanticTokenType};

#[derive(Debug, PartialEq, Eq, EnumIter, Display)]
#[repr(u32)]
pub enum TokenTypes {
    None,
    Keyword,
    Variable,
    Operator,
    BuiltinFunction,
    Number,
    String,
    Type,
}

impl From<TokenTypes> for SemanticTokenType {
    fn from(value: TokenTypes) -> Self {
        match value {
            TokenTypes::None => SemanticTokenType::new("none"),
            TokenTypes::Keyword => SemanticTokenType::KEYWORD,
            TokenTypes::Variable => SemanticTokenType::VARIABLE,
            TokenTypes::Operator => SemanticTokenType::OPERATOR,
            TokenTypes::BuiltinFunction => SemanticTokenType::new("builtin-functions"),
            TokenTypes::Number => SemanticTokenType::NUMBER,
            TokenTypes::String => SemanticTokenType::STRING,
            TokenTypes::Type => SemanticTokenType::TYPE,
        }
    }
}

#[derive(Debug, PartialEq, Eq, EnumIter, Display)]
#[repr(u32)]
pub enum TokenModifiers {
    None,
    Static,
}

impl From<TokenModifiers> for SemanticTokenModifier {
    fn from(value: TokenModifiers) -> Self {
        match value {
            TokenModifiers::None => SemanticTokenModifier::new("none"),
            TokenModifiers::Static => SemanticTokenModifier::STATIC,
        }
    }
}
