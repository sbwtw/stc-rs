use stc::parser::{ParserBuilder, StLexerBuilder};

fn main() {
    let f = "lib/src/test/test_decl_parse/test_base_types.st";
    let mut lexer = StLexerBuilder::new().build_file(f).unwrap();

    let parser = ParserBuilder::default().build();
    match parser.parse(&mut lexer) {
        Ok(r) => {
            println!("{:?}", r);
        }
        Err(e) => panic!("{}: {:?}", f, e),
    }
}
