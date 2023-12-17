use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{PartialAstIr, AstNodeParsable};
use crate::parser::ast::PartialAstIr::OBJECT_VARIABLE_DESTRUCTURING;
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::EXPECTED_ONEOF;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::element::DestructuringElement;
use crate::parser::parse::ParsableTokenStream;

#[derive(PartialEq, Debug, Clone)]
pub struct ObjectDestructureAstNode(pub Vec<DestructuringElement>);

impl AstNodeParsable for ObjectDestructureAstNode {
    // assumption: iterator index is currently ON the opening {
    fn parse(iter: &mut TokenParserIter, _: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        // val MyStruct {a, b: {c = 10}} = untyped_stuff
        let mut res = vec![];
        while let span = iter.next_skip_whitespace() {
            if span.is_none() {
                return Err(EXPECTED_ONEOF(iter.peek_prev().unwrap(), None, vec![TOKEN::COMMA, TOKEN::RIGHT_BRACE, TOKEN::COLON]))
            }
            let span = span.unwrap();
            if span.token == TOKEN::RIGHT_BRACE {
                break
            }
            if span.token == TOKEN::COMMA {
                continue
            }
            if span.token.is_btick_or_reg_ident() {
                let next = iter.peek_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_ONEOF(span, None, vec![TOKEN::COMMA, TOKEN::RIGHT_BRACE, TOKEN::COLON]))
                }
                let next = next.unwrap();
                if next.token == TOKEN::S2_ASSIGN {
                    iter.next_skip_whitespace(); //drop =
                    iter.next_skip_whitespace(); //drop next but we dont really drop it because the func below is inclusive
                    let rem = iter.remaining_inclusive();
                    let (sliced, ix) = rem.parse_into_ast_vec(vec![TOKEN::COMMA, TOKEN::RIGHT_BRACE])?;
                    iter.seek((ix - 1usize) as isize);
                    let decl = DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: Some(sliced),
                        children: None,
                        actual_binding_name: None
                    };
                    res.push(decl);
                    continue
                }
                if next.token == TOKEN::COLON {
                    iter.next_skip_whitespace().unwrap();
                    let left_brace_or_ident = iter.next_skip_whitespace();
                    if left_brace_or_ident.is_none() {
                        return Err(EXPECTED_ONEOF(next, None, vec![TOKEN::LEFT_BRACE]))
                    }
                    let left_brace_or_ident = left_brace_or_ident.unwrap();
                    if left_brace_or_ident.token.is_btick_or_reg_ident() {
                        let decl = DestructuringElement {
                            name: Some(span.token.get_ident_name()),
                            default_value: None,
                            children: None,
                            actual_binding_name: Some(left_brace_or_ident.token.get_ident_name())
                        };
                        res.push(decl);
                        continue
                    }
                    let children = ObjectDestructureAstNode::parse(iter, Either::Left(left_brace_or_ident))?;
                    let decl = DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: None,
                        children: Some(children),
                        actual_binding_name: None
                    };
                    res.push(decl);
                    continue
                }
                if next.token == TOKEN::RIGHT_BRACE {
                    res.push(DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: None,
                        children: None,
                        actual_binding_name: None
                    });
                    iter.next_skip_whitespace().unwrap();
                    break
                }
                if next.token == TOKEN::COMMA {
                    res.push(DestructuringElement {
                        name: Some(span.token.get_ident_name()),
                        default_value: None,
                        children: None,
                        actual_binding_name: None
                    });
                    iter.next_skip_whitespace().unwrap();
                    continue
                }

            }
            return Err(EXPECTED_ONEOF(span, Some(iter.peek_skip_whitespace().unwrap().token), vec![TOKEN::COMMA, TOKEN::RIGHT_BRACE, TOKEN::COLON]))
        }
        Ok(OBJECT_VARIABLE_DESTRUCTURING(ObjectDestructureAstNode(res)))
    }
}