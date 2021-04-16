mod parser;
mod ast;
mod utils;

use parser::lexer;
use ast::AstNode;

fn main() {
    println!("Hello, world!");

    let lexer = lexer::Lexer::new("2-3.0,3+5");
    let r = parser::st::ExprsParser::new().parse(lexer).unwrap();

    let mut stringify = utils::StringifyVisitor::new();
    r.accept(&mut stringify);

    println!("{:?}", r);
}
