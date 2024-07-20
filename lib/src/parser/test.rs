use crate::parser::{LalrpopParser, StLexerBuilder};

#[test]
pub fn test_parse_if_statement() {
    let st = "a := 1; if a >= 0 then a := 0; end_if";
    let lexer = StLexerBuilder::new().build_str(st);
    let parser = LalrpopParser::new();

    assert!(parser.parse(lexer).is_ok())
}
