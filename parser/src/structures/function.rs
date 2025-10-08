use common::iter::SmartIterator;
use common::LocationDataProvider;
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::structures::qualified_ident::{parse_qualified_ident, Identifier};
use crate::structures::type_expr::{parse_type_expression, TypeExpression};
use crate::TokenIter;

pub fn parse_func_decl(smart_iterator: &mut TokenIter, root: TokenSpan) -> Result<(), ParseError> {
    let start_location = smart_iterator.get_location();
    let (name_start, index) = smart_iterator
        .0
        .next()
        .ok_or(ParseError::ExpectedIdentifier(start_location, None))?;
    let name_start = name_start.clone();
    let qualified_name = parse_qualified_ident(smart_iterator, name_start)?;
    let args = parse_func_signature(smart_iterator, qualified_name)?;
    return Ok(())

}

pub fn parse_func_signature(smart_iterator: &mut TokenIter, name: Span<Identifier>) -> Result<(), ParseError> {
    let location = smart_iterator.get_location();
    let generics = if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_ANGLE) {
        Some(parse_generic_args(smart_iterator)?)
    } else {
        None
    };

    let location = smart_iterator.get_location();
    if smart_iterator.0.peek_is_fn(|x| x.get() != &Token::L_PAREN) {
        return Err(ParseError::ExpectedOneOf(location, vec![Token::L_PAREN, if generics.is_none() {Token::L_ANGLE} else {Token::L_PAREN}], None))
    }
    Ok(())
}

#[derive(Debug, Clone)]
pub struct GenericArg {
    name: String,
}

impl GenericArg {
    //TODO! rebuild this later to also turn the generic arg into a more easy to read format for later stages
    pub fn validate_expr(expr: &TypeExpression) -> Result<(), ParseError> {
        let arg = match expr {
            TypeExpression::Identifier(ident) => GenericArg::validate_ident(ident),
            TypeExpression::Extends(name, extension) => match name.get().as_ref().clone() {
                TypeExpression::Identifier(name) => GenericArg::validate_ident(&name),
                _ => Err(ParseError::InvalidGenericArgumentNameNaive())
            },
            TypeExpression::Ternary(name, _, falsy) => match name.get().as_ref().clone() {
                TypeExpression::Extends(name, _) => match name.get().as_ref().clone() {
                    TypeExpression::Identifier(name) => GenericArg::validate_ident(&name),
                    _ => Err(ParseError::InvalidGenericArgumentNameNaive())
                },
                _ => Err(ParseError::InvalidGenericArgumentNameNaive())
            },
            _ => Err(ParseError::InvalidGenericArgumentNameNaive())
        }?;

        Ok(())

    }

    fn validate_ident(ident: &Identifier) -> Result<GenericArg, ParseError> {
        match ident {
            Identifier::STATIC(_, _) | Identifier::INSTANCE(_, _) => Err(ParseError::InvalidGenericArgumentNameNaive()),
            Identifier::TAIL(value, ) => Ok(GenericArg {
                name: value.clone(),
            })
        }
    }
}


pub fn parse_generic_args(smart_iterator: &mut TokenIter) -> Result<Span<Vec<Span<TypeExpression>>>, ParseError> {
    let (angle_opener, _) = smart_iterator.0.next().unwrap();
    let angle_opener = angle_opener.clone();
    let mut constraints = vec![];
    loop {
        let constraint = parse_type_expression(smart_iterator)?;
        let valid = match constraint.get() {
            TypeExpression::Identifier(_) | TypeExpression::Ternary(_, _, _) | TypeExpression::Extends(_, _) => true,
            _ => false
        };
        GenericArg::validate_expr(constraint.get())?;
        let location  = smart_iterator.get_location();
        if !valid {
            return Err(ParseError::InvalidGeneric(location))
        }
        constraints.push(constraint);
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_ANGLE, None))?;
        let next = next.clone();
        if next.get() == &Token::R_ANGLE {
            return Ok(Span::new(angle_opener.start(), next.end(), constraints))
        }
        if next.get() != &Token::COMMA {
            let next = next.get().clone();
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedClosureOrComma(location, Token::R_ANGLE, Some(next)))
        }
    }
}


pub fn parse_args(smart_iterator: &mut TokenIter) -> Result<(), ParseError> {

}
pub fn parse_single_argument(smart_iterator: &mut TokenIter) -> Result<(), ParseError> {
    Ok(())
}