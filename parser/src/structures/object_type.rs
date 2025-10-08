use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::lexical_expr::{Expression};
use crate::structures::type_expr::{parse_type_expression, TypeExpression};
use crate::TokenIter;


#[derive(Debug, Clone)]
pub enum ObjectKeyType {
    Dynamic(Span<TypeExpression>),
    Label(String),
    SingleLiteral(Span<Expression>) // Only Expression::Literal
}

pub fn parse_object_dynamic_key_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeExpression>, ParseError> {
    let (bracket_open, _) = smart_iterator.0.next().unwrap();
    let bracket_open_start = bracket_open.start();
    let bracket_open = bracket_open.get().clone();
    if bracket_open != Token::L_BRACKET {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedToken(location, Token::L_BRACKET, Some(bracket_open)))
    }

    let key = parse_type_expression(smart_iterator)?;

    let location = smart_iterator.get_location();
    let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosure(location, Token::R_BRACKET, None))?;
    if next.get() != &Token::R_BRACKET {
        let next = next.get().clone();
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedClosure(location, Token::R_BRACKET, Some(next)));
    }

    Ok(Span::new(bracket_open_start, next.end(), key.get().clone()))
}