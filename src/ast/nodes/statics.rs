use crate::ast::nodes::expression::Expression;
use crate::ast::ToAst;
use crate::errors::ErrorReporter;
use crate::iter::wrap_iter;
use crate::tokens::span::{IntoSpan, Span};
use crate::tokens::{Literal, Token};

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
    pub raw: Vec<Option<String>>,
    pub expressions: Vec<Option<Span<Expression>>>
}

impl Token {
    // doesnt handle arrays
    pub fn as_static_unchecked(&self, reporter: &mut ErrorReporter) -> Option<StaticExpr> {
        Some(match self {
            Token::FLOAT(val) => StaticExpr::FLOAT(val.to_string()),
            Token::INT(val, is_bigint) => StaticExpr::INT(val.to_string(), *is_bigint),
            Token::BINARY_NUMBER(val, is_bigint) => StaticExpr::BIN(val.to_string(), *is_bigint),
            Token::OCTAL_NUMBER(val, is_bigint) => StaticExpr::OCT(val.to_string(), *is_bigint),
            Token::HEX_NUMBER(val, is_bigint) => StaticExpr::HEX(val.to_string(), *is_bigint),
            Token::EXP_NUMBER(val, is_bigint) => StaticExpr::EXP(val.to_string(), *is_bigint),
            Token::KEYWORD_TRUE => StaticExpr::TRUE,
            Token::KEYWORD_FALSE => StaticExpr::FALSE,
            Token::KEYWORD_NULL => StaticExpr::NULL,
            Token::LITERAL_DOUBLE(inner) => StaticExpr::STRING_DOUBLE(inner.as_expr_or_str_branch(reporter)?),
            Token::LITERAL_SINGLE(inner) => StaticExpr::STRING_SINGLE(inner.as_expr_or_str_branch(reporter)?),
            _ => unreachable!()
        })
    }
}

pub trait AsExpressionBranch {
    fn as_expr_or_str_branch(&self, reporter: &mut ErrorReporter) -> Option<StringTemplate>;
}

impl AsExpressionBranch for Literal {
    fn as_expr_or_str_branch(&self, reporter: &mut ErrorReporter) -> Option<StringTemplate> {
        let mut raw = vec![];
        let mut expressions = vec![];
        for elem in self {
            if elem.is_right() {
                let uw = elem.as_ref().unwrap_right().clone();
                let mut iter = wrap_iter(uw);
                let expr = iter.parse_expr(reporter)?;
                expressions.push(Some(expr));
                continue
            }
            if elem.is_left() {
                let uw = elem.as_ref().unwrap_left();
                raw.push(Some(uw.clone()));
            }
        }
        Some(StringTemplate {
            raw,
            expressions,
        })
    }
}