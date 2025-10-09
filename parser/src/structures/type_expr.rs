use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::function::parse_func_signature;
use crate::structures::lexical_expr::{parse_string_literal, Expression};
use crate::structures::object_type::{parse_object_dynamic_key_type, ObjectKeyType};
use crate::structures::qualified_ident::parse_ident;
use crate::TokenIter;

#[derive(Debug, Clone)]
pub enum TypeExpression {
    Identifier(Span<Expression>),
    FixedArrayType(Vec<Span<TypeExpression>>),
    ObjectType(Vec<(Span<ObjectKeyType>, Span<TypeExpression>)>),
    ConstBooleanTypeTrue,
    ConstBooleanTypeFalse,
    ConstNullType,
    ConstStringLiteral(Span<Expression>),
    ConstDecimalNumber(String),
    ConstBinaryNumber(String),
    ConstOctalNumber(String),
    ConstHexNumber(String),
    ConstFloatNumber(String),
    ConstructorType(()),
    And(Span<Box<TypeExpression>>, Span<Box<TypeExpression>>),
    Or(Span<Box<TypeExpression>>, Span<Box<TypeExpression>>),
    Ternary(Span<Box<TypeExpression>>, Span<Box<TypeExpression>>, Span<Box<TypeExpression>>),
    Extends(Span<Box<TypeExpression>>, Span<Box<TypeExpression>>),
}

pub fn parse_type_expression(smart_iterator: &mut TokenIter) -> Result<Span<TypeExpression>, ParseError> {
    let first_part = if smart_iterator.0.peek_is_fn(|t| t.get().is_ident()) {
        let identifier = parse_ident(smart_iterator)?;
        Span::new(identifier.start(), identifier.end(), TypeExpression::Identifier(identifier))
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_BRACKET) {
        parse_array_type(smart_iterator)?
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::L_CURLY) {
        parse_object_type(smart_iterator)?
    } else if smart_iterator.0.peek_is_fn(|x| x.get().is_literal()) {
        let expr = parse_string_literal(smart_iterator)?;
        Span::new(expr.start(), expr.end(), TypeExpression::ConstStringLiteral(expr))
    } else if smart_iterator.0.peek_is_fn(|x| x.get().is_number()) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let next = next.clone();
        let next_t = next.get().clone();
        let expr = match next_t {
            Token::B_NUMBER(num) => TypeExpression::ConstBinaryNumber(num),
            Token::D_NUMBER(num) => TypeExpression::ConstDecimalNumber(num),
            Token::O_NUMBER(num) => TypeExpression::ConstOctalNumber(num),
            Token::H_NUMBER(num) => TypeExpression::ConstHexNumber(num),
            Token::FD_NUMBER(num) => TypeExpression::ConstFloatNumber(num),
            _ => unreachable!()
        };
        Span::new(next.start(), next.end(), expr)
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_TRUE) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeExpression::ConstBooleanTypeTrue);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_FALSE) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeExpression::ConstBooleanTypeFalse);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_NULL) {
        let (next, _) = smart_iterator.0.next().unwrap();
        let spanned = Span::new(next.start(), next.end(), TypeExpression::ConstNullType);
        spanned
    } else if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::KEYWORD_NEW) {
        parse_constructor_type(smart_iterator)?
    } else {
        let location = smart_iterator.get_location();
        return Err(ParseError::ExpectedType(location, smart_iterator.0.peek().map(|x| x.get().clone())))
    };

    if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::PIPE || x.get() == &Token::AMP) {
        let (joiner, _) = smart_iterator.0.next().unwrap();
        let joiner = joiner.clone();
        let right = parse_type_expression(smart_iterator)?;
        let start = first_part.start();
        let end = right.end();
        let constraint = if joiner.get() == &Token::PIPE {
            TypeExpression::Or(first_part.boxed(), right.boxed())
        } else {
            TypeExpression::And(first_part.boxed(), right.boxed())
        };
        return Ok(Span::new(start, end, constraint))
    }


    if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::COLON) {
        let constraint = parse_type_expression(smart_iterator)?;
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::QMARK, None))?;
        let next = next.clone();
        if next.get() != &Token::QMARK {
            return Ok(Span::new(first_part.start(), constraint.end(), TypeExpression::Extends(first_part.boxed(), constraint.boxed())))
        }
        let truthy = parse_type_expression(smart_iterator)?;
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::COLON, None))?;
        let next = next.clone();
        if next.get() != &Token::COLON {
            let next = next.get().clone();
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedToken(location, Token::COLON, Some(next)))
        }
        let falsy = parse_type_expression(smart_iterator)?;
        return Ok(Span::new(first_part.start(), falsy.end(), TypeExpression::Ternary(first_part.boxed(), truthy.boxed(), falsy.boxed())))
    }

    Ok(first_part)

}


pub fn parse_array_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeExpression>, ParseError> {
    let (start, _) = smart_iterator.0.next().unwrap();
    let start = start.clone();
    let entry = parse_type_expression(smart_iterator)?;
    let location = smart_iterator.get_location();
    let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))?;
    if next.get() == &Token::R_BRACKET {
        return Ok(Span::new(start.start(), next.end(), TypeExpression::FixedArrayType(vec![entry])))
    }
    if next.get() == &Token::COMMA {
        let mut elements = vec![entry];
        loop {
            let entry = parse_type_expression(smart_iterator)?;
            elements.push(entry);
            let location = smart_iterator.get_location();
            let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_BRACKET, None))?;
            if next.get() == &Token::R_BRACKET {
                return Ok(Span::new(start.start(), next.end(), TypeExpression::FixedArrayType(elements)))
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

pub fn parse_object_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeExpression>, ParseError> {
    let (brace_open, _) = smart_iterator.0.next().unwrap();
    let brace_open = brace_open.clone();
    let mut elements = vec![];
    loop {
        if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::R_CURLY) {
            let (next, _) = smart_iterator.0.next().unwrap();
            let next = next.clone();
            return Ok(Span::new(brace_open.start(), next.end(), TypeExpression::ObjectType(elements)))
        }
        let key = if smart_iterator.0.peek_is_fn(|x| x.get().is_ident()) {
            let (key, _) = smart_iterator.0.next().unwrap();
            let key = key.clone();
            Span::new(key.start(), key.end(), ObjectKeyType::Label(key.get().get_identifier().unwrap()))
        } else {
            let key = parse_object_dynamic_key_type(smart_iterator)?;
            Span::new(key.start(), key.end(), ObjectKeyType::DynamicType(key))
        };
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedToken(location, Token::COLON, None))?;
        let next = next.clone();
        if next.get() != &Token::COLON {
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedToken(location, Token::COLON, Some(next.get().clone())));
        }
        let binding = parse_type_expression(smart_iterator)?;
        let location = smart_iterator.get_location();
        let (next, _) = smart_iterator.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, Token::R_CURLY, None))?;
        elements.push((key, binding));
        let next = next.clone();
        if next.get() == &Token::R_CURLY {
            return Ok(Span::new(brace_open.start(), next.end(), TypeExpression::ObjectType(elements)))
        }

        if next.get() != &Token::COMMA {
            let location = smart_iterator.get_location();
            return Err(ParseError::ExpectedClosureOrComma(location, Token::R_CURLY, Some(next.get().clone())))
        }
    }
}


pub fn parse_constructor_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeExpression>, ParseError> {
    let (new_token, _) = smart_iterator.0.next().unwrap();
    let new_token = new_token.clone();
    let args = parse_func_signature(smart_iterator, Span::zeroed_with(Expression::Nonce))?;
    todo!()
}