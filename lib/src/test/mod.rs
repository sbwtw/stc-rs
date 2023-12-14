use crate::parser::{self, StDeclarationParser, StLexerBuilder, StString};
use crate::prelude::*;
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = StDeclarationParser::new();

    for entry in fs::read_dir("src/test/test_decl_parse").unwrap() {
        let f = entry.unwrap().path();
        let code = fs::read_to_string(&f).unwrap();

        let lexer = StLexerBuilder::new().build_str(&code);
        match parser.parse(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{}: {:?}", f.display(), e),
        }
    }
}

#[test]
fn test_body_parse() {
    let parser = parser::StFunctionParser::new();

    for entry in fs::read_dir("src/test/test_body_parse").unwrap() {
        let f = entry.unwrap().path();
        let lexer = StLexerBuilder::new()
            .build_file(f.to_str().unwrap())
            .unwrap();

        match parser.parse(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{}: {:#?}", f.display(), e),
        }
    }
}

#[test]
fn test_scope_lookup() {
    let mgr = UnitsManager::new();
    let mgr_ui_app = mgr.clone();
    let app = ModuleContext::new(ModuleContextScope::Application);
    let app_id = app.read().id();
    mgr.write().add_context(app);
    mgr.write().set_active_application(Some(app_id));

    let app_ctx = mgr.write().get_context(app_id).unwrap();
    let global = StLexerBuilder::new().build_str("VAR_GLOBAL END_VAR VAR_GLOBAL g1: REAL; END_VAR");
    let global = StDeclarationParser::new().parse(global).unwrap();
    let _global_id = app_ctx.write().add_declaration(global);

    let test_func =
        StLexerBuilder::new().build_str("program prg: int VAR g1: BYTE; END_VAR end_program");
    let test_fun_decl = StDeclarationParser::new().parse(test_func).unwrap();
    let test_fun_decl_id = app_ctx.write().add_declaration(test_fun_decl);

    let scope = Scope::new(Some(mgr.clone()), Some(app_id), Some(test_fun_decl_id));
    let variable = scope.find_variable(&StString::new("g1"));

    assert!(variable.is_some());
}
