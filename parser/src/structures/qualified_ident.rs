use common::LocationDataProvider;
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::TokenIter;

#[derive(Debug, Clone)]
pub enum Identifier {
    STATIC(String, Option<Span<Box<Identifier>>>),
    INSTANCE(String, Option<Span<Box<Identifier>>>),
    TAIL(String, Option<Span<Box<Identifier>>>),
}

pub fn parse_qualified_ident(smart_iterator: &mut TokenIter, start: TokenSpan) -> Result<Span<Identifier>, ParseError> {
    let content = start.get().get_identifier().ok_or(ParseError::ExpectedIdentifier(smart_iterator.get_location(), smart_iterator.0.curr().map(|x| x.get().clone())))?;
    //TODO! generics
    if smart_iterator.0.peek_is_fn(|t| t.get() == &Token::PERIOD || t.get() == &Token::D_COLON) {
        let next_i = smart_iterator.0.next();
        let (next, _) = next_i.as_ref().unwrap();
        let next = next.clone();
        let next_connector = next.clone();
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedIdentifier(location, None))?;
        let next = next.clone();
        let next_ident = parse_qualified_ident(smart_iterator, next.clone())?;
        let ident = match next_connector.get() {
            &Token::PERIOD => Identifier::INSTANCE(content, Some(next_ident.boxed())),
            &Token::D_COLON => Identifier::STATIC(content, Some(next_ident.boxed())),
            _ => unreachable!(),
        };

        return Ok(Span::new(start.start(), next_connector.end(), ident))
    }
    Ok(Span::new(start.start(), start.end(), Identifier::TAIL(content, None)))
}