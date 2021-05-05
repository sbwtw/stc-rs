mod ast;
mod parser;
mod transform;
mod utils;

use parser::*;
use std::fs::OpenOptions;
use std::process::Command;

fn main() {
    let lexer = Lexer::new("2 + 3; a + 1; IF a + 3 THEN a - 3; END_IF");
    let r = parser::st::CompilationUnitsParser::new()
        .parse(lexer)
        .unwrap();

    // dump dot file
    {
        let mut f = OpenOptions::new()
            .write(true)
            .create(true)
            .truncate(true)
            .open("/home/test.dot")
            .unwrap();

        let mut graphviz = utils::GraphvizExporter::new(&mut f);
        graphviz.plot(&r);
    }

    // convert to svg
    {
        Command::new("dot")
            .args(&["-Tsvg", "/home/test.dot", "-o", "/home/test.svg"])
            .status()
            .expect("failed.");
    }

    println!("{:?}", r);
}
