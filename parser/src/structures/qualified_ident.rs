use common::LocationDataProvider;
use common::span::Span;
use lexer::token::Token;
use crate::errors::ParseError;
use crate::structures::lexical_expr::{parse_expr, Expression};
use crate::structures::type_expr::{parse_type_expression, TypeExpression};
use crate::TokenIter;


#[derive(Debug, Clone)]
pub enum ChainType {
    STATIC,
    INSTANCE,
}

#[derive(Debug, Clone)]
pub struct PathNodeDetails {
    pub optional: bool,
    pub asserted: bool,
    pub chain_ahead: Option<ChainType>,
    pub generic_args: Option<Vec<Span<TypeExpression>>>,
}

#[derive(Debug, Clone)]
pub enum PathNode {
    STATIC(String, PathNodeDetails),
    DYNAMIC(Span<Expression>, PathNodeDetails),
}


pub fn parse_ident(smart_iterator: &mut TokenIter) -> Result<Span<Expression>, ParseError> {
    let mut parts = vec![];
    let location = smart_iterator.get_location();
    let start = smart_iterator.0.curr().ok_or(ParseError::ExpectedIdentifier(location, None))?.start();
    loop {
        let starting_ident = smart_iterator.peek_and_next_if_fn(|x| x.is_ident() || x == &Token::L_BRACKET);
        if starting_ident.is_none() {
            break
        }
        let starting_ident = starting_ident.unwrap();
        let node_element = if starting_ident.get() == &Token::L_BRACKET {
            let expr = parse_expr(smart_iterator)?;
            smart_iterator.next_expect_token(Token::R_BRACKET)?;
            Ok(expr)
        } else {
            Err(starting_ident.get().get_identifier().unwrap())
        };

        let current_index = smart_iterator.0.get_next_index() - 1;
        let generic_args = if smart_iterator.peek_and_next_if_fn(|x| x == &Token::L_ANGLE).is_some() {
            //we might be in a generic
            let type_expr = parse_type_expression(smart_iterator);
            if type_expr.is_err() {
                None
            } else {
                let type_expr = type_expr.unwrap();
                if smart_iterator.peek_and_next_if_fn(|x| x == &Token::R_ANGLE).is_some() {
                    Some(vec![type_expr])
                } else if smart_iterator.peek_and_next_if_fn(|x| x == &Token::COMMA).is_some() {
                    let mut params = vec![type_expr];
                    let mut error = false;
                    loop {
                        let type_expr = parse_type_expression(smart_iterator);
                        if type_expr.is_err() {
                            error = true;
                            break
                        } else {
                            let type_expr = type_expr.unwrap();
                            params.push(type_expr);
                            if smart_iterator.peek_and_next_if_fn(|x| x == &Token::COMMA).is_some() {
                                continue
                            }
                        }
                    }
                    if error {
                        smart_iterator.0.seek(current_index);
                        None
                    } else {
                        Some(params)
                    }
                } else {
                    None
                }
            }
        } else {
            None
        };

        let assertion_or_qmark = smart_iterator.peek_and_next_if_fn(|x| x == &Token::QMARK || x == &Token::EXCL);
        let next_op = smart_iterator.peek_and_next_if_fn(|x| x == &Token::PERIOD || x == &Token::D_COLON);
        let end = smart_iterator.0.curr().unwrap().end();
        let chain_ahead = next_op.map_or(None, |x| match x.get() {
            &Token::PERIOD => Some(ChainType::INSTANCE),
            &Token::D_COLON => Some(ChainType::STATIC),
            _ => None,
        });
        let last = chain_ahead.is_none() && smart_iterator.0.peek_is_fn(|x| x.get() != &Token::L_BRACKET);
        let node_details = PathNodeDetails {
            optional: assertion_or_qmark.as_ref().map_or(false, |x| x.get() == &Token::QMARK),
            asserted: assertion_or_qmark.map_or(false, |x| x.get() == &Token::EXCL),
            generic_args,
            chain_ahead
        };
        let node = if node_element.is_err() {
            let err = node_element.unwrap_err();
            PathNode::STATIC(err, node_details)
        } else {
            PathNode::DYNAMIC(node_element.unwrap(), node_details)
        };
        parts.push(Span::new(starting_ident.start(), end, node));
        if last {
            break
        }
    }
    let last = parts.last().unwrap().end();
    Ok(Span::new(start, last, Expression::Ident(parts)))
}