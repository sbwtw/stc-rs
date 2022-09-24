use stc::ast::Statement;
use stc::context::{ModuleContext, ModuleContextScope, Scope, UnitsManager};
use stc::parser::{StDeclarationParser, StFunctionParser, StLexer};
use stc::transform::TypeAnalyzer;
use stc::utils;

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
    let app = ModuleContext::new(ModuleContextScope::Application);
    let app_id = app.read().id();
    mgr.add_context(app);

    let mut app = mgr.get_context(app_id).unwrap();
    let decl = StLexer::new("function test: int VAR a: INT; b: INT; END_VAR end_function");
    let decl = StDeclarationParser::new().parse(decl).unwrap();
    let decl_id = app.write().add_declaration(decl);

    let prg = StLexer::new("program prg: int VAR a: BYTE; END_VAR end_program");
    let prg = StDeclarationParser::new().parse(prg).unwrap();
    let _prg_id = app.write().add_declaration(prg);

    let global = StLexer::new("VAR_GLOBAL END_VAR VAR_GLOBAL 全局变量1: REAL; END_VAR");
    let global = StDeclarationParser::new().parse(global).unwrap();
    let _global_id = app.write().add_declaration(global);

    let body = StLexer::new("if a < 全局变量1 then prg.a := 1; else b := 2; end_if");
    let body = StFunctionParser::new().parse(body).unwrap();
    app.write().add_function(decl_id, body);

    let app = app.read();
    let fun = app.get_function(decl_id);

    let mut type_analyzer = TypeAnalyzer::new();

    // analysis declarations
    // for decl in app.get_declaration_by_id()

    // analysis function
    if let Some(f) = fun {
        let mut f = f.write().unwrap();

        let scope = Scope::new(
            Some(Arc::new(RwLock::new(mgr))),
            Some(app_id),
            Some(decl_id),
        );
        type_analyzer.analyze_statement(f.body_mut(), scope);

        display_ast(f.body());

        println!("{}", f.body());
    }
}
