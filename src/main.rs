mod parser;
mod ast;
mod utils;

use parser::lexer;
use ast::AstNode;

fn main() {
    println!("Hello, world!");

    let lexer = lexer::Lexer::new("2-3.0/3; 1+2;");
    let r = parser::st::CompilationUnitsParser::new().parse(lexer).unwrap();

    let mut stringify = utils::StringifyVisitor::new(std::io::stdout());
    r.accept(&mut stringify);

    println!("{:#?}", r);
}
