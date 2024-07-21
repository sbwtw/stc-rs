use stc::parser::{ParserBuilder, StLexerBuilder};
use std::fs;

fn main() {
    let f = "lib/src/test/test_body_parse/call_expression.st";
    let code = fs::read_to_string(f).unwrap();
    let lexer = StLexerBuilder::new().build_str(&code);

    let parser = ParserBuilder::new().build();
    match parser.parse_stmt(lexer) {
        Ok(r) => {
            println!("{}", r);
        }
        Err(e) => panic!("{}: {:?}", f, e),
    }
}
