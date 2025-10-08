use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::literal_expr::parse_literal_expr;
use crate::structures::qualified_ident::{parse_qualified_ident, Identifier};
use crate::TokenIter;

pub enum TypeConstraint {
    Identifier(Identifier),
    ArrayType(),
    ObjectType(),
    ConstBooleanTypeTrue,
    ConstBooleanTypeFalse,
    ConstNullType,
    ConstExpr(())
}

pub fn parse_type_constraint(smart_iterator: &mut TokenIter) -> Result<(), ParseError> {
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

    Ok(())
}

pub fn parse_object_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {

    Ok(())
}
u

pub fn parse_constructor_type(smart_iterator: &mut TokenIter) -> Result<Span<TypeConstraint>, ParseError> {
    Ok(())
}