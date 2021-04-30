mod parser;
mod ast;
mod utils;
mod transform;

use parser::*;
use ast::{AstNode, SyntaxTree};
use std::rc::Rc;

fn main() {
    println!("Hello, world!");

    let lexer = Lexer::new("2-3.0/3; -1+\"a\\\"s\\\"d\";");
    let syntax_tree = SyntaxTree::new();
    let r = parser::st::CompilationUnitsParser::new().parse(&syntax_tree, lexer).unwrap();

    let mut stringify = utils::StringifyVisitor::new(std::io::stdout());
    r.accept(&mut stringify);

    println!("{:?}", r);
}
