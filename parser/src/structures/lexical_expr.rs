use common::iter::SmartIterator;
use common::span::Span;
use lexer::token::{Token, TokenSpan};
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
pub enum Operand {
    EQ,
}

#[derive(Debug, Clone)]
pub enum Expression {
    Literal(Literal),
    Logical(Span<Box<Expression>>, Span<Operand>, Span<Box<Expression>>),
}

// if no logical expressions are involved, only e.g 10 or "KEY_{TEMPLATE}"
pub fn parse_literal_expr(smart_iterator: &mut TokenIter, start: TokenSpan) -> Result<Span<Expression>, ParseError> {
    return Ok(())
}


fn parse_expr_side(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let start = smart_iterator.next_or_expect_expr()?;
}


fn parse_logical(smart_iterator: &mut TokenIter) -> Result<Option<Span<Expression>>, ParseError> {

}

pub fn parse_expr(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let left = parse_expr_side(smart_iterator)?;
    let logical = parse_logical(smart_iterator)?;
    if logical.is_none() {
        return Ok(left);
    }
    let logical = logical.unwrap();
    let right_side = parse_expr_side(smart_iterator)?;

    // TOOD! MOCK!
    Ok(Span::new(left.start(), right_side.end(), Expression::Logical(left.boxed(), Span::new(0, 0Operand::EQ), right_side.boxed())))


}


pub fn parse_string_literal(smart_iterator: &mut TokenIter) -> Result<Span<Literal>, ParseError> {}
