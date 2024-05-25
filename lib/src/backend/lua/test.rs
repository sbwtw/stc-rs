use std::{fs, io::Write, process::Command};

use tempfile::tempdir;

use crate::{
    analysis::TypeAnalyzer,
    backend::{CodeGenBackend, CodeGenDriver, LuaBackend},
    parser::*,
    prelude::*,
};

fn eval_string<S1: AsRef<str>, S2: AsRef<str>>(decl: S1, body: S2) -> String {
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

    // Type analyze
    let ctx_id = ctx.read().id();
    let ctx_read = ctx.read();
    let mut type_analyzer = TypeAnalyzer::new();
    for proto in ctx_read.declarations() {
        let proto_read = proto.read().unwrap();
        let proto_id = proto_read.id();
        let fun = ctx_read.get_function(proto_read.id());

        if let Some(f) = fun {
            let mut f = f.write();

            let scope = Scope::new(Some(mgr.clone()), Some(ctx_id), Some(proto_id));
            type_analyzer.analyze_statement(f.parse_tree_mut(), scope);
        }
    }

    // Build
    let mut code_gen: CodeGenDriver<LuaBackend> = CodeGenDriver::new(mgr.clone(), ctx_id).unwrap();
    code_gen.build_application().expect("build app failed");

    let mut buf = vec![0u8; 0];
    code_gen
        .backend()
        .get_module_bytes(&mut buf)
        .expect("get module bytes failed");

    // Write to temporary file
    let dir = tempdir().unwrap();
    let fp = dir.path().join("test.o");
    let mut f = fs::OpenOptions::new()
        .write(true)
        .create(true)
        .truncate(true)
        .open(&fp)
        .unwrap();
    f.write_all(&buf).unwrap();

    // execute
    let lua_cmd = "lua5.4";
    let output = Command::new(lua_cmd)
        .arg(fp)
        .output()
        .expect("lua exec failed");

    // get outputs
    let s = String::from_utf8_lossy(&output.stdout).to_string();
    drop(f);

    s
}

#[test]
fn test_print() {
    let r = eval_string(
        "PROGRAM main: VAR a: int; END_VAR END_PROGRAM",
        "a := 1; print(a);",
    );
    assert_eq!(r, "1\n");
}
