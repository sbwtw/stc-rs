mod ast;
mod parser;
mod transform;
mod utils;

use parser::*;
use std::fs::OpenOptions;
use std::process::Command;

fn main() {
    let lexer = Lexer::new("IF a + 3 THEN a - 3; a + 2; ELSE a - 3; END_IF 1 + a; 1 - a;");

    let r = parser::st::CompilationUnitsParser::new()
        .parse(lexer)
        .unwrap();

    // dump dot file
    {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("test.dot")
            .unwrap();

        let mut graphviz = utils::GraphvizExporter::new(&mut f);
        graphviz.plot(r.as_ast_node());
    }

    // convert to svg
    {
        Command::new("dot")
            .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
            .status()
            .expect("failed.");
    }

    println!("{:?}", r);
}
