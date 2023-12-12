#![allow(dead_code)]
#![allow(unused_variables)]

pub mod analysis;
pub mod ast;
pub mod backend;
pub mod context;
pub mod parser;
pub mod utils;

pub mod prelude {
    pub use crate::ast::*;
    pub use crate::context::*;
    pub use crate::parser::StString;
}

#[cfg(test)]
mod test;
