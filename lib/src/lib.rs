#![allow(dead_code)]
#![allow(unused_variables)]

pub mod analysis;
pub mod ast;
pub mod codegen;
pub mod context;
pub mod parser;
pub mod utils;

#[cfg(test)]
mod test;
