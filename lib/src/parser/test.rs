use crate::parser::{ParserBuilder, StLexerBuilder};

#[test]
pub fn test_parse_if_statement() {
    let st = "a := 1; if a >= 0 then a := 0; end_if";
    let mut lexer = StLexerBuilder::new().build_str(st);
    let parser = ParserBuilder::default().build();

    assert!(parser.parse_stmt(&mut lexer).is_ok())
}
