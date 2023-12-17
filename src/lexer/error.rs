use crate::lexer::span::TokenSpan;
use crate::lexer::tokens::TOKEN;
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub enum LexError {
    EXPECTED_IDENT(TokenSpan),
    EXPECTED_ONEOF(TokenSpan, Vec<TOKEN>),
    EXPECTED_STR_GOT_TOK(TokenSpan, String, TOKEN),
    CUSTOM(TokenSpan, String)
}