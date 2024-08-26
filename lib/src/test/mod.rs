use crate::parser::{self, ParserBuilder, StLexerBuilder, StString};
use crate::prelude::*;
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = ParserBuilder::default().build();

    for entry in fs::read_dir("src/test/test_decl_parse").unwrap() {
        let f = entry.unwrap().path();
        let code = fs::read_to_string(&f).unwrap();

        let mut lexer = StLexerBuilder::new().build_str(&code);
        match parser.parse(&mut lexer) {
            Ok(_) => assert!(lexer.eof(), "{}", f.display()),
            Err(e) => panic!("{}: {:?}", f.display(), e),
        }
    }
}

#[test]
fn test_body_parse() {
    let parser = parser::ParserBuilder::default().build();

    for entry in fs::read_dir("src/test/test_body_parse").unwrap() {
        let f = entry.unwrap().path();
        let lexer = StLexerBuilder::new()
            .build_file(f.to_str().unwrap())
            .unwrap();

        match parser.parse_stmt(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{}: {:#?}", f.display(), e),
        }
    }
}

#[test]
fn test_literal_parse() {
    let parser = parser::ParserBuilder::default().build();

    let literal = parser.parse_literal_from_str("1");
    assert!(literal.is_ok());
}

#[test]
fn test_scope_lookup() {
    let mgr = UnitsManager::new();
    let mgr_ui_app = mgr.clone();
    let app = ModuleContext::new(ModuleKind::Application);
    let app_id = app.read().id();
    mgr.write().add_context(app);
    mgr.write().set_active_application(Some(app_id));

    let app_ctx = mgr.write().get_context(app_id).unwrap();
    let mut global =
        StLexerBuilder::new().build_str("VAR_GLOBAL END_VAR VAR_GLOBAL g1: REAL; END_VAR");
    let global = ParserBuilder::default().build().parse(&mut global).unwrap();
    let _global_id = app_ctx.write().add_declaration(global, Uuid::nil());

    let mut test_func =
        StLexerBuilder::new().build_str("program prg: int VAR g1: BYTE; END_VAR end_program");
    let test_fun_decl = ParserBuilder::default()
        .build()
        .parse(&mut test_func)
        .unwrap();
    let test_fun_decl_id = app_ctx.write().add_declaration(test_fun_decl, Uuid::nil());

    let scope = Scope::new(Some(mgr.clone()), Some(app_id), Some(test_fun_decl_id));
    let variable = scope.find_variable(&StString::new("g1"));

    assert!(variable.is_some());
}
