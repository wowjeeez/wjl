#[cfg(test)]
mod tests {
    use crate::lexer::{TOKEN, tokenize, TokenSpan};
    use crate::lexer::tests::util::assert_stream;
    use crate::lexer::TOKEN::*;


    #[test]
    fn should_parse_val_decl() {
        let buffer = String::from("val  x = 10;"); //notice the 2 spaces
        let tokens = tokenize(buffer).unwrap();
        assert_stream(tokens, vec![DECL_VAL, WHITESPACE, IDENTIFIER("x".to_string()), WHITESPACE, S2_ASSIGN, WHITESPACE, S2_INT_LITERAL(10), SEMI_COLON]);
    }
    #[test]
    fn should_parse_val_decl_with_type_and_destructure() {
        let buffer = String::from("val MyType {x: 10} = object"); //notice the 2 spaces
        let tokens = tokenize(buffer).unwrap();
        assert_stream(tokens, vec![DECL_VAL, WHITESPACE, IDENTIFIER("MyType".to_string()), WHITESPACE, LEFT_BRACE, IDENTIFIER("x".to_string()), COLON, WHITESPACE, S2_INT_LITERAL(10), RIGHT_BRACE, WHITESPACE, S2_ASSIGN, WHITESPACE, IDENTIFIER("object".to_string())]);
    }
    #[test]
    fn should_parse_val_decl_with_path_type_and_destructure() {
        let buffer = String::from("val MyType::inner {x: 10} = object"); //notice the 2 spaces
        let tokens = tokenize(buffer).unwrap();
        assert_stream(tokens, vec![DECL_VAL, WHITESPACE, IDENTIFIER("MyType".to_string()), S2_PATH_SEGMENT, IDENTIFIER("inner".to_string()), WHITESPACE, LEFT_BRACE, IDENTIFIER("x".to_string()), COLON, WHITESPACE, S2_INT_LITERAL(10), RIGHT_BRACE, WHITESPACE, S2_ASSIGN, WHITESPACE, IDENTIFIER("object".to_string())]);
    }
}