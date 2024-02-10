use crate::ast::nodes::expression::Expression;
use crate::tokens::span::Span;
use crate::tokens::Token;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum StaticExpr {
    STRING_SINGLE(StringTemplate),
    STRING_DOUBLE(StringTemplate),
    FLOAT(String),
    TRUE,
    FALSE,
    NULL,
    INT(String, bool),
    HEX(String, bool),
    EXP(String, bool),
    BIN(String, bool),
    OCT(String, bool),
    ARR(Vec<Span<Expression>>)
}
#[derive(Debug, Clone)]
struct StringTemplate {
    raw: Vec<Option<String>>,
    expressions: Vec<Option<Expression>>
}

impl Token {
    // doesnt handle arrays
    pub fn as_static_unchecked(&self) -> StaticExpr {
        match self {
            Token::FLOAT(val) => StaticExpr::FLOAT(val.to_string()),
            Token::INT(val, is_bigint) => StaticExpr::INT(val.to_string(), *is_bigint),
            Token::BINARY_NUMBER(val, is_bigint) => StaticExpr::BIN(val.to_string(), *is_bigint),
            Token::OCTAL_NUMBER(val, is_bigint) => StaticExpr::OCT(val.to_string(), *is_bigint),
            Token::HEX_NUMBER(val, is_bigint) => StaticExpr::HEX(val.to_string(), *is_bigint),
            Token::EXP_NUMBER(val, is_bigint) => StaticExpr::EXP(val.to_string(), *is_bigint),
            Token::KEYWORD_TRUE => StaticExpr::TRUE,
            Token::KEYWORD_FALSE => StaticExpr::FALSE,
            Token::KEYWORD_NULL => StaticExpr::NULL,
            Token::LITERAL_DOUBLE(inner) => StaticExpr::STRING_DOUBLE(todo!()),
            Token::LITERAL_SINGLE(inner) => StaticExpr::STRING_SINGLE(todo!()),
            _ => unreachable!()
        }
    }
}