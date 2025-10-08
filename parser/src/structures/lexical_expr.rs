use common::span::Span;
use lexer::token::TokenSpan;
use crate::errors::ParseError;
use crate::TokenIter;

#[derive(Debug, Clone)]
pub enum StringPart {
    String(String),
    Expr(Expression)
}


#[derive(Debug, Clone)]
pub enum Literal {
    DecimalNumber(String),
    OctalNumber(String),
    BinaryNumber(String),
    HexNumber(String),
    FloatNumber(String),
    SingleStringLiteral(Vec<StringPart>),
    DoubleStringLiteral(Vec<StringPart>),
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal)
}

// if no logical expressions are involved, only e.g 10 or "KEY_{TEMPLATE}"
pub fn parse_literal_expr(smart_iterator: &mut TokenIter, start: TokenSpan) -> Result<Span<Expression>, ParseError> {


    return Ok(())
}

pub fn parse_expr(smart_iterator: &mut TokenIter, start: TokenSpan) -> Result<Span<Expression>, ParseError> {
    Ok(())
}