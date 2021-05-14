use crate::parser::{self, Lexer};
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = parser::st::DeclarationParser::new();

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
