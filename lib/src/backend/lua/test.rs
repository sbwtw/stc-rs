use mlua::{ChunkMode, Lua};
use std::io::Write;
use std::process::Command;

use crate::backend::{CodeGenBackend, CodeGenDriver, LuaBackend};
use crate::{parser::*, prelude::*};

fn generate_module<S1: AsRef<str>, S2: AsRef<str>>(decl: S1, body: S2, writer: &mut dyn Write) {
    let mgr = UnitsManager::new();
    let ctx = ModuleContext::new(ModuleKind::Application);
    let lexer = StLexerBuilder::new().build_str(decl.as_ref());
    let decl = ParserBuilder::default().build().parse(lexer).unwrap();
    let fun_id = ctx.write().add_declaration(decl, Uuid::nil());

    let lexer = StLexerBuilder::new().build_str(body.as_ref());
    let body = ParserBuilder::default().build().parse_stmt(lexer).unwrap();
    ctx.write().add_function(fun_id, body);

    mgr.write().add_context(ctx.clone());
    mgr.write().set_active_application(Some(ctx.read().id()));

    // Build
    let ctx_id = ctx.read().id();
    let mut code_gen: CodeGenDriver<LuaBackend> = CodeGenDriver::new(mgr.clone(), ctx_id).unwrap();
    code_gen.build_application().expect("build app failed");

    code_gen
        .backend()
        .get_module_bytes(writer)
        .expect("get module bytes failed");
}

fn exec_binary<S1: AsRef<str>, S2: AsRef<str>>(decl: S1, body: S2) -> (String, String) {
    // Write to temporary file
    let mut f = tempfile::Builder::new()
        .tempfile()
        .expect("create temp file failed");

    // generate
    generate_module(decl, body, &mut f);

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
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: int; END_VAR END_PROGRAM",
        "a := 1; print(a);",
    );
    assert_eq!(r, "1\n", "Error: {}", e);

    // assignment twice
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: int; END_VAR END_PROGRAM",
        "a := 3; a := 2; print(a);",
    );
    assert_eq!(r, "2\n", "Error: {}", e);

    // print string
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := \"abc\"; print(a);",
    );
    assert_eq!(r, "abc\n", "Error: {}", e);
}

#[test]
fn test_add_print() {
    // R + R
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := a + a; print(a);",
    );
    assert_eq!(r, "4\n", "Error: {}", e);

    // R + K
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := a + 1; print(a);",
    );
    assert_eq!(r, "3\n", "Error: {}", e);

    // K + R
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: STRING; END_VAR END_PROGRAM",
        "a := 2; a := 1 + a; print(a);",
    );
    assert_eq!(r, "3\n", "Error: {}", e);

    // K + K
    let (r, e) = exec_binary(
        "PROGRAM main: VAR a: INT; END_VAR END_PROGRAM",
        "a := 2; a := 3 + 2; print(a);",
    );
    assert_eq!(r, "5\n", "Error: {}", e);
}

#[test]
fn test_add() {
    let decl = "PROGRAM main: VAR a: INT; END_VAR END_PROGRAM";
    let body = "a := 1 + 2;";

    // Generate to buffer
    let mut buf = vec![];
    generate_module(decl, body, &mut buf);

    let lua = Lua::new();
    assert!(lua.load(buf).set_mode(ChunkMode::Binary).exec().is_ok());

    let r = lua.globals().get::<_, i32>("a");
    assert_eq!(r.unwrap(), 3);
}

#[test]
fn test_if_statement() {
    let decl = "PROGRAM main: VAR a,b: INT; END_VAR END_PROGRAM";
    let body = "\
a := 1; \
if a = 1 then \
    b := a + 1; \
    a := 0; \
end_if";

    // Generate to buffer
    let mut buf = vec![];
    generate_module(decl, body, &mut buf);

    let lua = Lua::new();
    assert!(lua.load(buf).set_mode(ChunkMode::Binary).exec().is_ok());

    let r = lua.globals().get::<_, i32>("a");
    assert_eq!(r.unwrap(), 0);
    let r = lua.globals().get::<_, i32>("b");
    assert_eq!(r.unwrap(), 2);

    let decl = "PROGRAM main: VAR a: INT; END_VAR END_PROGRAM";
    let body = "\
a := 0; \
if a = 1 then \
    a := a + 1; \
    a := a + 1; \
end_if";

    // Generate to buffer
    let mut buf = vec![];
    generate_module(decl, body, &mut buf);

    let lua = Lua::new();
    assert!(lua.load(buf).set_mode(ChunkMode::Binary).exec().is_ok());

    let r = lua.globals().get::<_, i32>("a");
    assert_eq!(r.unwrap(), 0);
}
