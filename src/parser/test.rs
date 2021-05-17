use crate::ast::*;
use crate::parser::*;
use crate::utils::AstHasher;

#[test]
fn test_parse_function() {
    let lexer = Lexer::new("function test_fun : VAR_GLOBAL a,b ,c: INT; END_VAR END_FUNCTION");

    let fun = st::DeclarationParser::new().parse(lexer).unwrap();
    let fun = fun.as_any().downcast_ref::<FunctionDeclaration>().unwrap();

    assert_eq!(fun.name(), "test_fun");
    assert!(matches!(fun.class(), &DeclareClass::Function));

    let variables = fun.variables();
    assert_eq!(variables.len(), 3);
    assert_eq!(variables[0].name(), "a");
    assert_eq!(variables[1].origin_name(), "b");
    assert!(matches!(
        variables[2].scope_class(),
        VariableScopeClass::Global,
    ));

    let lexer = Lexer::new(
        "function test_fun : VAR_GLOBAL a,b ,c: INT; END_VAR VAR Bx1: INT; END_VAR END_FUNCTION",
    );

    let fun = st::DeclarationParser::new().parse(lexer).unwrap();
    let fun = fun.as_any().downcast_ref::<FunctionDeclaration>().unwrap();

    let variables = fun.variables();
    assert_eq!(variables.len(), 4);
    assert_eq!(variables[3].origin_name(), "Bx1");
    assert!(matches!(
        variables[3].scope_class(),
        VariableScopeClass::None,
    ));
    assert!(matches!(
        variables[3].ty().unwrap().type_class(),
        TypeClass::Int
    ));
}

fn hash_for_code<S: AsRef<str>>(s: S) -> Option<u64> {
    let parser = st::StFunctionParser::new();
    let lexer = Lexer::new(s.as_ref());
    let fun = parser.parse(lexer).ok()?;

    let mut hasher = AstHasher::crc32();
    hasher.add(fun.as_ast_node());
    Some(hasher.hash())
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
}
