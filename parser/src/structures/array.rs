use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::lexical_expr::{parse_expr, Expression};
use crate::TokenIter;



pub fn parse_array_decl(smart_iterator: &mut TokenIter) -> Result<Span<Vec<Span<Expression>>>, ParseError> {
    let location = smart_iterator.get_location();
    let (opening, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::L_CURLY, None))?;
    let opening = opening.clone();
    if opening.get() != &Token::L_BRACKET {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedToken(location, Token::L_BRACKET, Some(opening.get().clone())))
    }
    let mut entries = vec![];

    loop {
        if let Some(next) = smart_iterator.ensure_then_next(Token::R_BRACKET) {
            return Ok(Span::new(opening.start(), next.end(), entries))
        }
        let expr = parse_expr(smart_iterator)?;
        entries.push(expr);
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))?;
        let next = next.clone();
        if next.get() == &Token::R_BRACKET {
            continue
        }
        if next.get() == &Token::COMMA {
            continue
        }
        let location = smart_iterator.get_location();
        //TODO! spread syntax
        return Err(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, Some(next.get().clone())))
    }
}