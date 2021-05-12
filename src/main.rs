mod ast;
mod parser;
mod transform;
mod utils;

use parser::*;
use std::fs::OpenOptions;
use std::process::Command;

fn main() {
    let lexer = Lexer::new("function test_fun : VAR_GLOBAL a,b ,c: INT; END_VAR END_FUNCTION");

    let r = parser::st::FunctionDeclarationParser::new()
        .parse(lexer)
        .unwrap();

    println!("{:?}", r);

    // // dump dot file
    // {
    //     let mut f = OpenOptions::new()
    //         .write(true)
    //         .create(true)
    //         .truncate(true)
    //         .open("test.dot")
    //         .unwrap();
    //
    //     let mut graphviz = utils::GraphvizExporter::new(&mut f);
    //     graphviz.plot(r.as_ast_node());
    // }
    //
    // // convert to svg
    // {
    //     Command::new("dot")
    //         .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
    //         .status()
    //         .expect("failed.");
    // }
    //
    // println!("{}", r.as_ast_node());
}
