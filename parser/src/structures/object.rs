use common::LocationDataProvider;
use common::span::Span;
use common::tokio::time::Sleep;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::lexical_expr::{parse_expr, Expression};
use crate::structures::object_type::ObjectKeyType;
use crate::TokenIter;

pub fn parse_object_dynamic_key(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let (bracket_open, _) = smart_iterator.0.next().unwrap();
    let bracket_open_start = bracket_open.start();
    let bracket_open = bracket_open.get().clone();
    if bracket_open != Token::L_BRACKET {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedToken(location, Token::L_BRACKET, Some(bracket_open)))
    }

    let key = parse_expr(smart_iterator)?;

    let location = smart_iterator.get_location();
    let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosure(location, Token::R_BRACKET, None))?;
    if next.get() != &Token::R_BRACKET {
        let next = next.get().clone();
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedClosure(location, Token::R_BRACKET, Some(next)));
    }

    Ok(Span::new(bracket_open_start, next.end(), key.get().clone()))
}

#[derive(Debug, Clone)]
pub enum ObjectField {
    Auto(Span<String>),
    Bound(Span<String>, Span<Expression>),
    Dynamic(Span<Expression>, Span<Expression>),
}



pub fn parse_object_decl(smart_iterator: &mut TokenIter) -> Result<Span<Vec<Span<ObjectField>>>, ParseError> {
    let location = smart_iterator.get_location();
    let (opening, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::L_CURLY, None))?;
    let opening = opening.clone();
    if opening.get() != &Token::L_CURLY {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedToken(location, Token::L_CURLY, Some(opening.get().clone())))
    }
    let mut entries = vec![];
    loop {
        if let Some(next) = smart_iterator.ensure_then_next(Token::R_CURLY) {
            return Ok(Span::new(opening.start(), next.end(), entries))
        }
        let (key, key_span) = if smart_iterator.0.peek_is_fn(|x| x.get().is_ident()) {
            let (ident, _) = smart_iterator.0.next().unwrap();
            (ObjectKeyType::Label(ident.get().get_identifier().unwrap()), ident.clone())
        } else {
            let key = parse_object_dynamic_key(smart_iterator)?;
            (ObjectKeyType::DynamicRuntime(key), Span::zeroed())
        };
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosure(location, Token::R_CURLY, None))?;
        let next = next.clone();
        if next.get() == &Token::COMMA {
            if let ObjectKeyType::Label(ident) = key {
                entries.push(Span::new(key_span.start(), key_span.end(), ObjectField::Auto(Span::new(key_span.start(), key_span.end(), ident))));
                continue
            } else {
                let location = smart_iterator.get_location();
                return Err(ParseError::InvalidAutoAssign(location))
            }
        }

        if next.get() == &Token::COLON {
          let expr = parse_expr(smart_iterator)?;
           if let ObjectKeyType::DynamicRuntime(key_expr) = key {
               entries.push(Span::new(key_expr.start(), expr.end(), ObjectField::Dynamic(key_expr, expr)));
           } else if let ObjectKeyType::Label(label) = key {
               entries.push(Span::new(key_span.start(), expr.end(), ObjectField::Bound(Span::new(key_span.start(), key_span.end(), label), expr)));
           }
        }

        if next.get() == &Token::R_CURLY {
            continue
        }
        //TODO! default value (also fix it for the argument parsing because it will not move the loop ahead when a default assignment is found altough it should

        //TODO! ... spread syntax
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedClosureOrComma(location, Token::R_CURLY, Some(next.get().clone())))

    }
}