use stc::parser::{ParserBuilder, StLexerBuilder};
use std::fs;

fn main() {
    let f = "lib/src/test/test_decl_parse/test_parse_prg.st";
    let code = fs::read_to_string(f).unwrap();
    let lexer = StLexerBuilder::new().build_str(&code);

    let parser = ParserBuilder::new().build();
    match parser.parse(lexer) {
        Ok(r) => {
            println!("{:?}", r);
        }
        Err(e) => panic!("{}: {:?}", f, e),
    }
}
