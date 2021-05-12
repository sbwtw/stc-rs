use crate::ast::{FunctionDeclaration, FunctionType};
use crate::parser::*;

#[test]
fn test_parse_function() {
    let lexer = Lexer::new("function test_fun : VAR_GLOBAL a,b ,c: INT; END_VAR END_FUNCTION");

    let fun = st::FunctionDeclarationParser::new().parse(lexer).unwrap();
    let fun = fun.as_any().downcast_ref::<FunctionDeclaration>().unwrap();

    assert_eq!(fun.name(), "test_fun");
    assert!(matches!(fun.ty(), &FunctionType::Fun));

    let variables = fun.variables();
    assert_eq!(variables.len(), 3);
    assert_eq!(variables[0].name(), "a");
    assert_eq!(variables[1].origin_name(), "b");
}
