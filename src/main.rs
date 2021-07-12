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
    // let mut mgr = UnitsManager::new();
    // let mut app = ModuleContext::new(ModuleContextScope::Application);
    // let ctx_id = app.id();
    //
    // let decl = StLexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    // let decl = parser::StDeclarationParser::new().parse(decl).unwrap();
    // let decl_id = app.add_declaration(decl);
    //
    // let body = StLexer::new("a.b := 2 < b OR c = d + NOT b;");
    // let body = parser::StFunctionParser::new().parse(body).unwrap();
    // app.add_function(decl_id, body);
    // let fun = app.get_function(decl_id);
    // mgr.add_context(Arc::new(RwLock::new(app)));
    //
    // if let Some(f) = fun {
    //     let mut f = f.write().unwrap();
    //     let mut analyzer = TypeAnalyzer::new();
    //
    //     let scope = Scope::new(
    //         Some(Arc::new(RwLock::new(mgr))),
    //         Some(ctx_id),
    //         Some(decl_id),
    //     );
    //     analyzer.analyze(f.as_ast_node_mut(), scope);
    //

    let lexer = StLexer::new("2-3.0/3; -1+2**3;");

    // parse
    let mut r = StFunctionParser::new().parse(lexer).unwrap();

    println!("{:?}", r);

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
        graphviz.plot(r.as_ast_node());
    }

    // convert to svg
    {
        Command::new("dot")
            .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
            .status()
            .expect("failed.");
    }
}
