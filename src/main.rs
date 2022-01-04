mod ast;
mod codegen;
mod context;
mod parser;
mod transform;
mod utils;

#[cfg(test)]
mod test;

use crate::ast::Statement;
use crate::context::{ModuleContext, ModuleContextScope, Scope, UnitsManager};
use crate::transform::TypeAnalyzer;
use parser::*;
use std::fs::OpenOptions;
use std::process::Command;
use std::sync::{Arc, RwLock};

fn display_ast(statement: &Statement) {
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
        graphviz.plot_statement(statement);
    }

    // convert to svg
    {
        Command::new("dot")
            .args(&["-Tsvg", "test.dot", "-o", "test.svg"])
            .status()
            .expect("failed.");
    }
}

fn main() {
    let mut mgr = UnitsManager::new();
    let mut app = ModuleContext::new(ModuleContextScope::Application);
    let ctx_id = app.id();

    let decl = StLexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let decl = parser::StDeclarationParser::new().parse(decl).unwrap();
    let decl_id = app.add_declaration(decl);

    let global = StLexer::new("VAR_GLOBAL END_VAR VAR_GLOBAL c: REAL; END_VAR");
    let global = parser::StDeclarationParser::new().parse(global).unwrap();
    let _global_id = app.add_declaration(global);

    let body = StLexer::new("if a < c then a := 1; else b := 2; end_if");
    let body = parser::StFunctionParser::new().parse(body).unwrap();
    app.add_function(decl_id, body);
    let fun = app.get_function(decl_id);
    mgr.add_context(Arc::new(RwLock::new(app)));

    if let Some(f) = fun {
        let mut f = f.write().unwrap();
        let mut type_analyzer = TypeAnalyzer::new();

        let scope = Scope::new(
            Some(Arc::new(RwLock::new(mgr))),
            Some(ctx_id),
            Some(decl_id),
        );
        type_analyzer.analyze_statement(&mut f, scope);

        display_ast(&f);

        println!("{}", &f);
    }
}
