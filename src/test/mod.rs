use crate::parser::{self, StLexer};
use std::fs;

#[test]
fn test_decl_parse() {
    let parser = parser::StDeclarationParser::new();

    for entry in fs::read_dir("src/test/test_decl_parse").unwrap() {
        let f = entry.unwrap().path();
        let code = fs::read_to_string(&f).unwrap();

        let lexer = StLexer::new(&code);
        match parser.parse(lexer) {
            Ok(_) => {}
            Err(e) => panic!("{}: {:?}", f.display(), e),
        }
    }
}
