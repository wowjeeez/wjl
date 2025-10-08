use common::span::Span;
use lexer::token::TokenSpan;
use crate::errors::ParseError;
use crate::structures::type_constraint::TypeConstraint;
use crate::TokenIter;

pub fn parse_literal_expr(smart_iterator: &mut TokenIter, start: TokenSpan) -> Result<(), ParseError> {


    return Ok(())
}

#[derive(Debug, Clone)]
pub enum SingleLiteral {

}

pub fn parse_single_literal(smart_iterator: &mut TokenIter, lit: TokenSpan) -> Result<Span<SingleLiteral>, ParseError> {

}