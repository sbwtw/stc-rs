use crate::ast::*;
use crate::parser::*;
use crate::utils::{AstHasher, Crc32Hasher};

#[test]
fn test_parse_function() {
    let lexer = StLexer::new("function test_fun : VAR_INPUT a,b ,c: INT; END_VAR END_FUNCTION");

    let fun = StDeclarationParser::new().parse(lexer).unwrap();

    assert_eq!(fun.identifier(), "test_fun");
    assert!(matches!(fun.kind, DeclKind::Fun(_)));

    if let DeclKind::Fun(f) = fun.kind {
        let variables = f.parameters();
        assert_eq!(variables.len(), 3);
        assert_eq!(variables[0].name(), "a");
        assert_eq!(variables[1].origin_name(), "b");
        assert!(matches!(variables[2].scope(), VariableFlags::INPUT));
    }

    let lexer = StLexer::new(
        "function test_fun : VAR_INOUT a,b ,c: INT; END_VAR VAR Bx1: INT; END_VAR END_FUNCTION",
    );

    let fun = StDeclarationParser::new().parse(lexer).unwrap();
    assert_eq!(fun.identifier(), "test_fun");
    assert!(matches!(fun.kind, DeclKind::Fun(_)));

    if let DeclKind::Fun(f) = fun.kind {
        let variables = f.parameters();
        assert_eq!(variables.len(), 4);
        assert_eq!(variables[3].origin_name(), "Bx1");
        assert!(matches!(variables[3].scope(), VariableFlags::NONE));
        assert!(matches!(
            variables[3].ty().unwrap().type_class(),
            TypeClass::Int
        ));
    }
}

fn hash_for_code<S: AsRef<str>>(s: S) -> Option<u64> {
    let parser = StFunctionParser::new();
    let lexer = StLexer::new(s.as_ref());
    let fun = parser.parse(lexer).ok()?;

    let mut hasher = AstHasher::new(Crc32Hasher::new());
    Some(hasher.calc_statement(&fun))
}

fn stringify_code<S: AsRef<str>>(s: S) -> Option<String> {
    let parser = StFunctionParser::new();
    let lexer = StLexer::new(s.as_ref());
    let fun = parser.parse(lexer).ok()?;

    Some(format!("{}", fun))
}

#[test]
fn test_parse() {
    let code1 = "a.b := (c);";
    let code2 = "a.b := c;\n".to_owned();

    assert_eq!(stringify_code(code1), Some(code2));
}

#[test]
fn test_precedence() {
    let code1 = "a + b / c;";
    let code2 = "a + (b / c);";
    assert_eq!(hash_for_code(code1), hash_for_code(code2));

    let code1 = "a + -b / c;";
    let code2 = "a + ((-b) / c);";
    assert_eq!(hash_for_code(code1), hash_for_code(code2));

    let code1 = "a := b - a * c;";
    let code2 = "a := (b - (a * c));";
    assert_eq!(hash_for_code(code1), hash_for_code(code2));

    let code1 = "a := a < 5 | a > 3;";
    let code2 = "a := (a < 5) | a > 3;";
    assert_eq!(hash_for_code(code1), hash_for_code(code2));

    let code1 = "a := a & 5 | a ** -3 MOD b = 1;";
    let code2 = "a := (a & 5) | (((a ** (-3)) MOD b) = 1);";
    assert_eq!(hash_for_code(code1), hash_for_code(code2));
}
