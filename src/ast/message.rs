use crate::ast::*;

pub enum MessageCategory {
    TypeAnalysis(MessageID),
}

pub enum MessageID {}

pub trait HasMessage {}

#[macro_export]
macro_rules! impl_has_message {
    ($ty:ident, $storage:ident) => {
        impl HasMessage for $ty {}
    };
}
