mod ast;
mod context;
mod parser;
mod transform;
mod utils;

#[cfg(test)]
mod test;

use crate::context::{ModuleContext, ModuleContextScope, Scope, UnitsManager};
use crate::transform::TypeAnalyzer;
use parser::*;
use std::fs::OpenOptions;
use std::process::Command;
use std::sync::{Arc, RwLock};

fn main() {
    let mut mgr = UnitsManager::new();
    let mut app = ModuleContext::new(ModuleContextScope::Application);
    let ctx_id = app.id();

    let decl = Lexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let mut decl = parser::st::DeclarationParser::new().parse(decl).unwrap();
    let decl_id = app.add_declaration(decl);

    let body = Lexer::new("a := a + b;");
    let mut body = parser::st::StFunctionParser::new().parse(body).unwrap();
    app.add_function(decl_id, body);
    let fun = app.get_function(decl_id);
    mgr.add_context(Arc::new(RwLock::new(app)));

    if let Some(f) = fun {
        let mut f = f.write().unwrap();
        let mut analyzer = TypeAnalyzer::new();

        let scope = Scope::new(
            Some(Arc::new(RwLock::new(mgr))),
            Some(ctx_id),
            Some(decl_id),
        );
        analyzer.analyze(f.as_ast_node_mut(), scope);

        // graphviz
        // dump dot file
        {
            let mut out = OpenOptions::new()
                .write(true)
                .create(true)
                .truncate(true)
                .open("test.dot")
                .unwrap();

            let mut graphviz = utils::GraphvizExporter::new(&mut out);
            graphviz.plot(f.as_ast_node());
        }

        // convert to svg
        {
            Command::new("dot")
                .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
                .status()
                .expect("failed.");
        }
    }

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
    //
    // println!("{}", r.as_ast_node());
}
