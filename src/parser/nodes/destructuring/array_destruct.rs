use std::vec;
use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{PartialAstIr, AstNodeParsable};
use crate::parser::ast::PartialAstIr::ARRAY_VARIABLE_DESTRUCTURING;
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::EXPECTED_COMMA_OR_CLOSING;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::destructuring::element::DestructuringElement;
use crate::parser::parse::ParsableTokenStream;

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayDestructureAstNode(pub Vec<DestructuringElement>);

impl AstNodeParsable for ArrayDestructureAstNode {
    // assumption: iterator index is currently ON the opening [
    fn parse(mut iter: &mut TokenParserIter, _: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let mut nodes = vec![];

        while let span = iter.next_skip_whitespace() {
            if span.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(iter.peek_prev().unwrap(), None, vec![TOKEN::RIGHT_BRACKET]))
            }
            let span = span.unwrap();

            if span.token.is_btick_or_reg_ident() {
                let next = iter.next_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_COMMA_OR_CLOSING(span, None, vec![TOKEN::RIGHT_BRACKET]))
                }
                let next = next.unwrap();
                if next.token == TOKEN::COMMA || next.token == TOKEN::RIGHT_BRACKET {
                    let decl = DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: None,
                        children: None,
                        actual_binding_name: None
                    };
                    nodes.push(decl);
                    if next.token == TOKEN::RIGHT_BRACKET {
                        break
                    } else {
                        continue
                    }
                }

                if next.token == TOKEN::S2_ASSIGN {
                    // default value
                    let rem = iter.remaining_inclusive();
                    let (default, iter_pos) = rem.parse_into_ast_vec(vec![TOKEN::COMMA, TOKEN::RIGHT_BRACKET])?;
                    iter.seek(iter_pos as isize);
                    let decl = DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: Some(default),
                        children: None,
                        actual_binding_name: None
                    };
                    nodes.push(decl);

                    let curr = iter.peek_n(0);
                    if curr.is_none() {
                        return Err(EXPECTED_COMMA_OR_CLOSING(next, None, vec![TOKEN::RIGHT_BRACKET]))
                    }
                    let curr = curr.unwrap();
                    if curr.token == TOKEN::RIGHT_BRACKET {
                        break
                    }
                    continue
                }
            }
            if span.token == TOKEN::LEFT_BRACKET {
                let nested = ArrayDestructureAstNode::parse(&mut iter, Either::Left(span.clone()))?;
                let decl = DestructuringElement {
                    name: None,
                    default_value: None,
                    children: Some(nested),
                    actual_binding_name: None
                };
                nodes.push(decl);
                let next = iter.peek_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_COMMA_OR_CLOSING(span, None, vec![TOKEN::RIGHT_BRACKET]))
                }
                let next = next.unwrap();
                if next.token == TOKEN::RIGHT_BRACKET {
                    break
                }
                continue
            }
            return Err(EXPECTED_COMMA_OR_CLOSING(span, None, vec![TOKEN::RIGHT_BRACKET]))

        }
        return Ok(ARRAY_VARIABLE_DESTRUCTURING(ArrayDestructureAstNode(nodes)))
    }
}
