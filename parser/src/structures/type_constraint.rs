use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::literal_expr::{parse_literal_expr, parse_single_literal};
use crate::structures::object::{parse_object_dynamic_key, ObjectKey};
use crate::structures::qualified_ident::{parse_qualified_ident, Identifier};
use crate::TokenIter;

pub enum TypeConstraint {
    Identifier(Identifier),
    FixedArrayType(Vec<Span<TypeConstraint>>),
    ObjectType(Vec<(Span<ObjectKey>, Span<TypeConstraint>)>),
    ConstBooleanTypeTrue,
    ConstBooleanTypeFalse,
    ConstNullType,
    ConstExpr(())
}

pub fn parse_type_constraint(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {
    let first_part = if smart_iterator.0.peek_is_fn(|t| t.get().is_ident()) {
        let (start, _) = smart_iterator.0.next().unwrap();
        let start = start.clone();
        let identifier = parse_qualified_ident(smart_iterator, start)?;
        let ident = identifier.get().clone().clone();

        Span::new(identifier.start(), identifier.end(), TypeConstraint::Identifier(ident))
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_BRACKET) {
        parse_array_type(smart_iterator)?
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_CURLY) {
        parse_object_type(smart_iterator)?
    } else if smart_iterator.0.peek_is_fn(|x| x.get().is_any_literal()) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let next = next.clone();
        let expr = parse_literal_expr(smart_iterator, next.clone())?;
        Span::new(next.start(), next.end(), TypeConstraint::ConstExpr(expr))

    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_TRUE) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeConstraint::ConstBooleanTypeTrue);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_FALSE) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeConstraint::ConstBooleanTypeFalse);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_NULL) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeConstraint::ConstNullType);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_NEW) {
        parse_constructor_type(smart_iterator)?
    } else {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedType(location, smart_iterator.0.peek().map(|x| x.get().clone())))
    };


    Ok(())
}


pub fn parse_array_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {
    let (start, _) = smart_iterator.0.next().unwrap();
    let start = start.clone();
    let entry = parse_type_constraint(smart_iterator)?;
    let location = smart_iterator.get_location();
    let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))?;
    if next.get() == &Token::R_BRACKET {
        return Ok(Span::new(start.start(), next.end(), TypeConstraint::FixedArrayType(vec![entry])))
    }
    if next.get() == &Token::COMMA {
        let mut elements = vec![entry];
        loop {
            let entry = parse_type_constraint(smart_iterator)?;
            elements.push(entry);
            let location = smart_iterator.get_location();
            let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))?;
            if next.get() == &Token::R_BRACKET {
                return Ok(Span::new(start.start(), next.end(), TypeConstraint::FixedArrayType(elements)))
            }
            if next.get() != &Token::COMMA {
                let location = smart_iterator.get_location();
                return Err(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))
            }
        }
    }
    let next = next.get().clone();
    let location = smart_iterator.get_location();
    Err(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, Some(next)))
}

pub fn parse_object_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {
    let (brace_open, _) = smart_iterator.0.next().unwrap();
    let brace_open = brace_open.clone();
    let mut elements = vec![];
    loop {
        let key = if smart_iterator.0.peek_is_fn(|x| x.get().is_any_literal() || x.get().is_ident()) {
            let (key, _) = smart_iterator.0.next().unwrap();
            if key.get().is_ident() {
                Span::new(key.start(), key.end(), ObjectKey::Label(key.get().get_identifier().unwrap()))
            } else {
                let literal = parse_single_literal(smart_iterator, key.clone())?;
                Span::new(literal.start(), literal.end(), ObjectKey::SingleLiteral(literal.get().clone()))
            }
        } else {
            let key = parse_object_dynamic_key(smart_iterator)?;
            Span::new(key.start(), key.end(), ObjectKey::Dynamic(key.get().clone()))
        };
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::COLON, None))?;
        let next = next.clone();
        if next.get() != &Token::COLON {
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedToken(location, Token::COLON, Some(next.get().clone())));
        }
        let binding = parse_type_constraint(smart_iterator)?;
        elements.push((key, binding));
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_CURLY, None))?;
        let next = next.clone();
        if next.get() == &Token::R_CURLY {
            return Ok(Span::new(brace_open.start(), next.end(), TypeConstraint::ObjectType(elements)))
        }
        if next.get() != &Token::COMMA {
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedClosureOrComma(location, Token::R_CURLY, Some(next.get().clone())))
        }
    }
}


pub fn parse_constructor_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {
    Ok(())
}