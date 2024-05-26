use std::process::Command;

use crate::{
    backend::{CodeGenBackend, CodeGenDriver, LuaBackend},
    parser::*,
    prelude::*,
};

fn eval_string<S1: AsRef<str>, S2: AsRef<str>>(decl: S1, body: S2) -> (String, String) {
    let mgr = UnitsManager::new();
    let ctx = ModuleContext::new(ModuleContextScope::Application);
    let lexer = StLexerBuilder::new().build_str(decl.as_ref());
    let decl = StDeclarationParser::new().parse(lexer).unwrap();
    let fun_id = ctx.write().add_declaration(decl, Uuid::nil());

    let lexer = StLexerBuilder::new().build_str(body.as_ref());
    let body = StFunctionParser::new().parse(lexer).unwrap();
    ctx.write().add_function(fun_id, body);

    mgr.write().add_context(ctx.clone());
    mgr.write().set_active_application(Some(ctx.read().id()));

    // Build
    let ctx_id = ctx.read().id();
    let mut code_gen: CodeGenDriver<LuaBackend> = CodeGenDriver::new(mgr.clone(), ctx_id).unwrap();
    code_gen.build_application().expect("build app failed");

    // Write to temporary file
    let mut f = tempfile::Builder::new()
        .tempfile()
        .expect("create temp file failed");
    code_gen
        .backend()
        .get_module_bytes(&mut f)
        .expect("get module bytes failed");

    // execute
    let lua_cmd = "lua";
    let output = Command::new(lua_cmd)
        .arg(f.path())
        .output()
        .expect("lua exec failed");

    // get outputs
    let r = String::from_utf8_lossy(&output.stdout).to_string();
    let e = String::from_utf8_lossy(&output.stderr).to_string();

    (r, e)
}

#[test]
fn test_print() {
    // assignment
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: int; END_VAR END_PROGRAM",
        "a := 1; print(a);",
    );
    assert_eq!(r, "1\n", "Error: {}", e);

    // assignment twice
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: int; END_VAR END_PROGRAM",
        "a := 3; a := 2; print(a);",
    );
    assert_eq!(r, "2\n", "Error: {}", e);

    // print string
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := \"abc\"; print(a);",
    );
    assert_eq!(r, "abc\n", "Error: {}", e);
}

#[test]
fn test_add() {
    // R + R
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := a + a; print(a);",
    );
    assert_eq!(r, "4\n", "Error: {}", e);

    // R + K
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := a + 1; print(a);",
    );
    assert_eq!(r, "3\n", "Error: {}", e);

    // K + R
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := 1 + a; print(a);",
    );
    assert_eq!(r, "3\n", "Error: {}", e);

    // K + K
    let (r, e) = eval_string(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := 3 + 2; print(a);",
    );
    assert_eq!(r, "5\n", "Error: {}", e);
}
