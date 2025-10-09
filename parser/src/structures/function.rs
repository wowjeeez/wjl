use std::process::id;
use common::iter::SmartIterator;
use common::LocationDataProvider;
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::structures::array::parse_array_decl;
use crate::structures::block::CodeBlock;
use crate::structures::lexical_expr::{parse_expr, Expression};
use crate::structures::object::parse_object_decl;
use crate::structures::qualified_ident::{parse_ident};
use crate::structures::type_expr::{parse_type_expression, TypeExpression};
use crate::structures::variable::VariableName;
use crate::TokenIter;


pub fn parse_async_func_decl(smart_iterator: &mut TokenIter) -> Result<(), ParseError> {
    Ok(())
}

pub fn parse_func_decl(smart_iterator: &mut TokenIter, root: TokenSpan) -> Result<(), ParseError> {
    let qualified_name = parse_ident(smart_iterator)?;
    let args = parse_func_signature(smart_iterator, qualified_name)?;
    return Ok(())

}
#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    name: Span<Expression>,
    generics: Option<Span<Vec<Span<TypeExpression>>>>,
    args: Span<Vec<(VariableName, Span<TypeExpression>, Option<Span<Expression>>)>>,
    return_type: Option<Span<TypeExpression>>,
    body: CodeBlock
}

pub fn parse_func_signature(smart_iterator: &mut TokenIter, name: Span<Expression>) -> Result<(), ParseError> {
    let location = smart_iterator.get_location();
    let generics = if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_ANGLE) {
        Some(parse_generic_args(smart_iterator)?)
    } else {
        None
    };

    let location = smart_iterator.get_location();
    let next = smart_iterator.0.next();
    if next.is_none() {
        return Err(ParseError::ExpectedOneOf(location, vec![Token::L_PAREN, if generics.is_none() {Token::L_ANGLE} else {Token::L_PAREN}], None))
    }
    let (next, _) = next.unwrap();
    let next = next.clone();
    if next.get() != &Token::L_PAREN {
        return Err(ParseError::ExpectedOneOf(location, vec![Token::L_PAREN, if generics.is_none() {Token::L_ANGLE} else {Token::L_PAREN}], Some(next.get().clone())))
    }

    let args = parse_args(smart_iterator, next)?;

    Ok(())
}

#[derive(Debug, Clone)]
pub struct GenericArg {
    name: String,
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


pub fn parse_args(smart_iterator: &mut TokenIter, paren: TokenSpan) -> Result<Span<Vec<(VariableName, Span<TypeExpression>, Option<Span<Expression>>)>>, ParseError> {
    let mut args = vec![];
    loop {
        if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::R_PAREN) {
            let (next, _) = smart_iterator.0.next().unwrap();
            return Ok(Span::new(paren.start(), next.end(), args))
        }
        let location = smart_iterator.get_location();
        let binding = if smart_iterator.0.peek_is_fn(|x| x.get().is_ident()) {
            let ident = parse_ident(smart_iterator)?;
            Ok(VariableName::Ident(ident))
        } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_CURLY) {
            parse_object_decl(smart_iterator);
            Ok(VariableName::ObjectDestructuring())
        } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_BRACKET) {
            parse_array_decl(smart_iterator);
            Ok(VariableName::ArrayDestructuring())
        } else {
            return Err(ParseError::ExpectedOneOf(location, vec![Token::L_BRACKET, Token::L_CURLY, Token::IDENT("".to_string())], smart_iterator.0.peek().as_ref().map(|x| x.get().clone())))
        }?;

        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::COLON, None))?;
        let next = next.clone();
        if next.get() != &Token::COLON {
            let next = next.get().clone();
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedToken(location, Token::COLON, Some(next)))
        }

        let ty = parse_type_expression(smart_iterator)?;
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_PAREN, None))?;
        let next = next.clone();
        let default_value = if next.get() == &Token::ASSIGN {
            Some(parse_expr(smart_iterator)?)
        } else {None};

        args.push((binding, ty, default_value));
        if next.get() != &Token::COMMA && next.get() != &Token::R_PAREN {
            let next = next.get().clone();
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedClosureOrComma(location, Token::R_PAREN, Some(next)))
        }

    }
}