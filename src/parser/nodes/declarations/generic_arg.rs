use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::ast::PartialAstIr::GENERIC_ARG;
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::{EXPECTED_COMMA_OR_CLOSING, EXPECTED_ONEOF};
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::generic_decl::{TypeContextExpression, TypeContextIfExpr};
use crate::parser::nodes::identifier::IdentifierAstNode;

#[derive(Clone, Debug, PartialEq)]
pub struct GenericArgAstNode {
    expr: Option<TypeContextExpression>,
    infer: bool
}

impl AstNodeParsable for GenericArgAstNode {

    // iter on <
    fn parse(iter: &mut TokenParserIter, strt: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let mut nodes = vec![];
        while let span = iter.next_skip_whitespace() {
            if span.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(strt.left().unwrap(), None, vec![TOKEN::RIGHT_ANGLE]));
            }
            let span = span.unwrap();

            let node = match span.token {
                TOKEN::IDENTIFIER(_) | TOKEN::BACKTICK_LITERAL(_) => {
                    let ident = IdentifierAstNode::parse(iter, Either::Left(span))?.c_get_ident_ref_content();
                    if ident.len() == 1 && ident.first().unwrap().name == "_".to_string() {
                        GenericArgAstNode {
                            infer: true,
                            expr: None
                        }
                    } else {
                        GenericArgAstNode {
                            infer: false,
                            expr: Some(TypeContextExpression {
                                static_type: Some(ident),
                                if_expr: None,
                                static_value: None,
                                right_condition_op: None
                            })
                        }
                    }
                },
                TOKEN::KEYWORD_IF => {
                    let if_expr = TypeContextIfExpr::parse(iter)?;
                    GenericArgAstNode {
                        infer: false,
                        expr: Some(TypeContextExpression {
                            static_type: None,
                            if_expr: Some(if_expr),
                            static_value: None,
                            right_condition_op: None
                        })
                    }
                },
                _ => if span.token.is_static_value() {
                        GenericArgAstNode {
                            infer: false,
                            expr: Some(TypeContextExpression {
                                static_type: None,
                                if_expr: None,
                                static_value: Some(span.token),
                                right_condition_op: None
                            })
                        }
                    } else {
                        return Err(EXPECTED_ONEOF(strt.left().unwrap(), Some(span.token), vec![TOKEN::ERR_PLACEHOLDER_TYPE_EXPR]));
                    }

            };
            let next = iter.next_skip_whitespace();
            if next.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(strt.left().unwrap(), None, vec![TOKEN::RIGHT_ANGLE]));
            }
            let next = next.unwrap();
            nodes.push(node);
            if next.token == TOKEN::RIGHT_ANGLE {
                break
            }
            if next.token == TOKEN::COMMA {
                continue
            }
            return Err(EXPECTED_COMMA_OR_CLOSING(strt.left().unwrap(), Some(next.token), vec![TOKEN::RIGHT_ANGLE]));
        }


        Ok(GENERIC_ARG(nodes))
    }
}