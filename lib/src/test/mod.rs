use crate::parser::{self, StLexerBuilder};
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = parser::StDeclarationParser::new();

    for entry in fs::read_dir("src/test/test_decl_parse").unwrap() {
        let f = entry.unwrap().path();
        let code = fs::read_to_string(&f).unwrap();

        let lexer = StLexerBuilder::new().build(&code);
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
        let code = fs::read_to_string(&f).unwrap();

        let lexer = StLexerBuilder::new().build(&code);
        match parser.parse(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{}: {:#?}", f.display(), e),
        }
    }
}
