mod ast;
mod context;
mod parser;
mod transform;
mod utils;

#[cfg(test)]
mod test;

use crate::context::{ModuleContext, ModuleContextScope, UnitsManager};
use crate::transform::TypeAnalyzer;
use parser::*;
use std::fs::OpenOptions;
use std::process::Command;

fn main() {
    let mut mgr = UnitsManager::new();
    let mut app = ModuleContext::new(ModuleContextScope::Application);

    let decl = Lexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let mut decl = parser::st::DeclarationParser::new().parse(decl).unwrap();
    app.add_declaration(decl);

    // let app = mgr.add_context(app);

    let body = Lexer::new("a + b;");
    let mut body = parser::st::StFunctionParser::new().parse(body).unwrap();

    // let mut analyzer = TypeAnalyzer::new();
    // analyzer.analyze()

    // let lexer = Lexer::new("a + b; if 3.0 + b then a - b; end_if");
    //
    // // parse
    // let mut r = parser::st::StFunctionParser::new().parse(lexer).unwrap();
    // // type analyze
    // let mut analyzer = TypeAnalyzer::new();
    // analyzer.analyze(r.as_ast_node_mut());
    //
    // println!("{:?}", r);
    //
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
