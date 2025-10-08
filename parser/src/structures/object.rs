use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::literal_expr::{parse_literal_expr, SingleLiteral};
use crate::structures::qualified_ident::{parse_qualified_ident, Identifier};
use crate::TokenIter;

#[derive(Debug, Clone)]
pub enum ObjectDynamicKey {
    Identifier(Identifier),
    LiteralExpr(())
}
#[derive(Debug, Clone)]
pub enum ObjectKey {
    Dynamic(ObjectDynamicKey),
    Label(String),
    SingleLiteral(SingleLiteral)
}

pub fn parse_object_dynamic_key(smart_iterator: &mut TokenIter) -> Result<Span<ObjectDynamicKey>, ParseError> {
    let (bracket_open, _) = smart_iterator.0.next().unwrap();
    let bracket_open = bracket_open.clone();
    let location = smart_iterator.get_location();
    let next = smart_iterator.0.next();
    let (next, _) = next.ok_or(ParseError::ExpectedIdentifierOrLiteralExpression(location, None))?;
    let next = next.clone();

    let key = if next.get().is_ident() {
        let key = parse_qualified_ident(smart_iterator, next)?;
        ObjectDynamicKey::Identifier(key.get().clone())
    } else if next.get().is_any_literal() {
        let key = parse_literal_expr(smart_iterator, next)?;
        ObjectDynamicKey::LiteralExpr(key)
    } else {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedIdentifierOrLiteralExpression(location, None));
    };

    let location = smart_iterator.get_location();
    let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosure(location, Token::R_BRACKET, None))?;
    if next.get() != &Token::R_BRACKET {
        let next = next.get().clone();
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedClosure(location, Token::R_BRACKET, Some(next)));
    }

    Ok(Span::new(bracket_open.start(), next.end(), key))
}