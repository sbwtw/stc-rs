use stc::parser::{ParserBuilder, StLexerBuilder};

fn main() {
    env_logger::init();

    let f = "lib/src/test/test_decl_parse/test_array_decl.st";
    let mut lexer = StLexerBuilder::new().build_file(f).unwrap();

    let parser = ParserBuilder::default().build();
    match parser.parse_decl(&mut lexer) {
        Ok(r) => {
            println!("{:?}", r);
        }
        Err(e) => panic!("{}: {:?}", f, e),
    }
}
