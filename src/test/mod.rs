use crate::parser::{self, Lexer};
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = parser::DeclarationParser::new();

    for entry in fs::read_dir("src/test/test_decl_parse").unwrap() {
        let f = entry.unwrap().path();
        let code = fs::read_to_string(f).unwrap();

        let lexer = Lexer::new(&code);
        match parser.parse(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{:?}", e),
        }
    }
}

#[test]
#[ignore]
fn test_decl_parse_text() {
    let lexer = Lexer::new("TYPE TestEnum: (E1, E2 := 10, E3) DINT; END_TYPE");
    let parser = parser::DeclarationParser::new();

    let r = parser.parse(lexer).unwrap();
    println!("{}", r.as_ref());
}
