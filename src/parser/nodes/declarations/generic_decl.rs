use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::ast::PartialAstIr::GENERIC_DECL;
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::{EXPECTED_COMMA_OR_CLOSING, EXPECTED_ONEOF, GE_CONDITION_REQUIRED, GENERIC_CANNOT_BE_EMPTY};
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::identifier::IdentifierAstNode;


#[derive(Clone, Debug, PartialEq)]
pub struct GenericDeclarationAstNode {
    exact: bool,
    default_value: Option<GenericConstraintBranch>,
    constraints: Option<GenericConstraintBranch>,
    name: String,
    optional: bool
}

#[derive(Clone, Debug, PartialEq)]
pub enum GenericConstraintJoinOp {
    AND,
    OR
}

#[derive(Clone, Debug, PartialEq)]
pub struct GenericConstraintBranch {
    right_join_op: Option<GenericConstraintJoinOp>,
    elements: Vec<Either<GenericConstraintBranch, TypeContextExpression>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeContextIfExpr {
    condition: TypeContextIfExprConditionBranch,
    body: GenericConstraintBranch,
    else_body: GenericConstraintBranch
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeContextIfExprConditionBranch {
    right_join_op: Option<GenericConstraintJoinOp>,
    elements: Vec<Either<TypeContextIfExprCondition, TypeContextIfExprConditionBranch>>
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeContextIfExprCondition {
    name: Vec<IdentifierAstNode>,
    constraints: GenericConstraintBranch,
    right_op: Option<GenericConstraintJoinOp>
}

#[derive(Clone, Debug, PartialEq)]
pub struct TypeContextExpression {
    pub(crate) static_type: Option<Vec<IdentifierAstNode>>,
    pub(crate) static_value: Option<TOKEN>, //true, false, int, float or string lit
    pub(crate) if_expr: Option<TypeContextIfExpr>,
    pub(crate) right_condition_op: Option<GenericConstraintJoinOp>
}

impl TypeContextIfExpr {
    //assuming that iter is on the KEYWORD_IF
    pub fn parse(iter: &mut TokenParserIter) -> Result<TypeContextIfExpr, ParseError> {
        let condition = TypeContextIfExpr::parse_if_condition_branch(iter, vec![TOKEN::KEYWORD_TYPE_THEN])?;
        let body = parse_generic_branch_until_unmatched(iter, vec![TOKEN::KEYWORD_ELSE])?;
        let else_body = parse_generic_branch_until_unmatched(iter, vec![TOKEN::KEYWORD_TYPE_END])?;
        Ok(TypeContextIfExpr {
            condition,
            body,
            else_body
        })
    }

    //iter should be on IF or (
    fn parse_if_condition_branch(iter: &mut TokenParserIter, stop_on_unmatched: Vec<TOKEN>) -> Result<TypeContextIfExprConditionBranch, ParseError> {
        let mut elements: Vec<Either<TypeContextIfExprCondition, TypeContextIfExprConditionBranch>> = vec![];
        while let span = iter.next_skip_whitespace() {
            if span.is_none() {
                return Err(GE_CONDITION_REQUIRED(iter.peek_n(0).unwrap()))
            }
            let span = span.unwrap();
            if stop_on_unmatched.contains(&span.token) {
                break
            }

            if span.token == TOKEN::LEFT_PAREN {
                let mut child = TypeContextIfExpr::parse_if_condition_branch(iter, vec![TOKEN::RIGHT_PAREN])?;
                let next = iter.next_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_ONEOF(iter.peek_n(0).unwrap(), None, stop_on_unmatched))
                }
                let next = next.unwrap();
                if next.token == TOKEN::S2_AND || next.token == TOKEN::S2_OR {
                    child.right_join_op = Some(match next.token {
                       TOKEN::S2_AND => GenericConstraintJoinOp::AND,
                        TOKEN::S2_OR => GenericConstraintJoinOp::OR,
                        _ => unreachable!()
                    });
                }
                let is_child_none = child.right_join_op.is_none();
                elements.push(Either::Right(child));
                if is_child_none {
                    if stop_on_unmatched.contains(&next.token) {
                        break
                    }
                }
                continue
            }
            if span.token.is_btick_or_reg_ident() {
                let ident = IdentifierAstNode::parse(iter, Either::Left(span.clone()))?;
                let next = iter.next_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_ONEOF(span, None, vec![TOKEN::COLON]));
                }
                let next = next.unwrap();
                if next.token != TOKEN::COLON {
                    return Err(EXPECTED_ONEOF(span, Some(next.token), vec![TOKEN::COLON]));
                }
                let mut until_um = vec![TOKEN::S2_OR, TOKEN::S2_AND];
                until_um.append(&mut stop_on_unmatched.clone());
                let constraint = parse_generic_branch_until_unmatched(iter, until_um)?;
                iter.seek(-1);
                let cond_or_end = iter.next_skip_whitespace();
                if cond_or_end.is_none() {
                    return Err(EXPECTED_ONEOF(next, None, vec![TOKEN::COLON]));
                }
                let cond_or_end = cond_or_end.unwrap();
                let mut constraint = TypeContextIfExprCondition {
                    constraints: constraint,
                    name: ident.c_get_ident_ref_content(),
                    right_op: None
                };
                if cond_or_end.token == TOKEN::S2_OR || cond_or_end.token == TOKEN::S2_AND {
                    constraint.right_op =  Some(match cond_or_end.token {
                        TOKEN::S2_AND => GenericConstraintJoinOp::AND,
                        TOKEN::S2_OR => GenericConstraintJoinOp::OR,
                        _ => unreachable!()
                    });
                }
                let is_cond_none = constraint.right_op.is_none();
                elements.push(Either::Left(constraint));
                if is_cond_none {
                    if stop_on_unmatched.contains(&cond_or_end.token) {
                        break
                    }
                }
                continue
            }
        }

        Ok(TypeContextIfExprConditionBranch {
            right_join_op: None,
            elements
        })



    }
}

impl AstNodeParsable for GenericDeclarationAstNode {
    // assuming that iter is now on the opening <
    fn parse(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> { // u know its getting serious when I start naming variables like this
        let mut args = vec![];
        while let exact_or_ident = iter.next_skip_whitespace() {
            if exact_or_ident.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(start.left().unwrap(), None, vec![TOKEN::RIGHT_ANGLE]));
            }
            let mut exact_or_ident = exact_or_ident.unwrap();
            if exact_or_ident.token == TOKEN::RIGHT_ANGLE && args.len() == 0 {
                return Err(GENERIC_CANNOT_BE_EMPTY(start.left().unwrap()));
            }
            let exact = if exact_or_ident.token == TOKEN::KEYWORD_EXACT {
                let next = iter.next_skip_whitespace();
                if next.is_none() || !next.as_ref().unwrap().token.is_btick_or_reg_ident() {
                    return Err(EXPECTED_ONEOF(exact_or_ident, next.map_or(None, |x| Some(x.token)), vec![TOKEN::IDENTIFIER("".to_string())]))
                }
                let next = next.unwrap();
                exact_or_ident = next;
                true
            } else {
                false
            };
            if !exact_or_ident.token.is_btick_or_reg_ident() {
                return Err(EXPECTED_ONEOF(exact_or_ident.clone(), Some(exact_or_ident.token), vec![TOKEN::IDENTIFIER("".to_string())]))
            }

            let param_name = exact_or_ident.token.get_ident_name();
            let colon_or_closing_or_comma_or_qmark_or_assign = iter.next_skip_whitespace();
            if colon_or_closing_or_comma_or_qmark_or_assign.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(exact_or_ident, None, vec![TOKEN::RIGHT_ANGLE]));
            }
            let colon_or_closing_or_comma_or_qmark_or_assign = colon_or_closing_or_comma_or_qmark_or_assign.unwrap();

            let mut colon_or_qmark_or_assign = colon_or_closing_or_comma_or_qmark_or_assign; //just for more clarity
            let optional = if colon_or_qmark_or_assign.token == TOKEN::QUESTION_MARK {
                let colon_or_assign = iter.next_skip_whitespace();
                if colon_or_assign.is_none() {
                    return Err(EXPECTED_ONEOF(colon_or_qmark_or_assign.clone(), None, vec![TOKEN::COLON, TOKEN::QUESTION_MARK, TOKEN::S2_ASSIGN, TOKEN::COMMA, TOKEN::RIGHT_ANGLE]))
                }
                let colon_or_assign = colon_or_assign.unwrap();
                colon_or_qmark_or_assign = colon_or_assign;
                true
            } else {
                false
            };
            let mut colon_or_or_assign = colon_or_qmark_or_assign;
            let constraints = if colon_or_or_assign.token == TOKEN::COLON {
                let res = parse_generic_branch_until_unmatched(iter, vec![TOKEN::RIGHT_ANGLE, TOKEN::COMMA])?;
                iter.seek(-1);
                let next = iter.next_skip_whitespace();
                if next.is_none() {
                    return Err(EXPECTED_ONEOF(colon_or_or_assign.clone(), None, vec![TOKEN::COLON, TOKEN::QUESTION_MARK, TOKEN::S2_ASSIGN, TOKEN::COMMA, TOKEN::RIGHT_ANGLE]))
                }
                colon_or_or_assign = next.unwrap();
                Some(res)
            } else {
                None
            };
            if colon_or_or_assign.token == TOKEN::COMMA || colon_or_or_assign.token == TOKEN::RIGHT_ANGLE {
                args.push(GenericDeclarationAstNode {
                    name: param_name,
                    exact,
                    default_value: None,
                    constraints,
                    optional,
                });
                if colon_or_or_assign.token == TOKEN::RIGHT_ANGLE {
                    break
                }
                continue
            }
        }

        Ok(GENERIC_DECL(args))
    }
}


// assuming that iter is currently on the opening of the branch, either :, ( or =
fn parse_generic_branch_until_unmatched(iter: &mut TokenParserIter, until_unmatched: Vec<TOKEN>) -> Result<GenericConstraintBranch, ParseError> {
    let mut branch_elements: Vec<Either<GenericConstraintBranch, TypeContextExpression>> = vec![];

    while let span = iter.next_skip_whitespace() {
        if span.is_none() {
            return Err(EXPECTED_COMMA_OR_CLOSING(iter.peek_prev_skip_whitespace().unwrap(), None, until_unmatched))
        }
        let span = span.unwrap();
        if until_unmatched.contains(&span.token) {
            break
        }
        if branch_elements.last().is_some() {
            let last = branch_elements.last().unwrap();
            let has_join_cond = match last {
                Either::Left(l) => l.right_join_op.is_some(),
                Either::Right(r) => r.right_condition_op.is_some()
            };
            if !has_join_cond {
                return Err(EXPECTED_ONEOF(span.clone(), Some(span.token), until_unmatched))
            }
        }
        if span.token == TOKEN::LEFT_PAREN {
            let mut nested_group = parse_generic_branch_until_unmatched(iter, vec![TOKEN::RIGHT_PAREN])?;
            let closing_or_op = iter.next_skip_whitespace();
            if closing_or_op.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(span, None, until_unmatched))
            }
            let closing_or_op = closing_or_op.unwrap();
            if closing_or_op.token == TOKEN::PIPE || closing_or_op.token == TOKEN::SUM {
                nested_group.right_join_op = Some(match closing_or_op.token {
                    TOKEN::PIPE => GenericConstraintJoinOp::OR,
                    TOKEN::SUM => GenericConstraintJoinOp::AND,
                    _ => unreachable!()
                });
            }
            let is_nested_none = nested_group.right_join_op.is_none();
            branch_elements.push(Either::Left(nested_group));
            if until_unmatched.contains(&closing_or_op.token) {
                break
            } else if is_nested_none {
                return Err(EXPECTED_COMMA_OR_CLOSING(span, Some(closing_or_op.token.clone()), until_unmatched))
            } else {
                continue
            }
        }

        if span.token.is_btick_or_reg_ident() || span.token.is_static_value() || span.token == TOKEN::KEYWORD_IF {
            let static_type = if span.token.is_btick_or_reg_ident() {Some(IdentifierAstNode::parse(iter, Either::Left(span.clone()))?.c_get_ident_ref_content())} else {None};
            let static_value = if span.token.is_static_value() {
                Some(span.token.clone())
            } else {None};
            let if_expr = if span.token == TOKEN::KEYWORD_IF {
                Some(TypeContextIfExpr::parse(iter)?)
            } else {
                None
            };
            let closing_or_op = iter.next_skip_whitespace();
            if closing_or_op.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(span, None, until_unmatched))
            }
            let closing_or_op = closing_or_op.unwrap();
            let right_op = match closing_or_op.token {
                TOKEN::PIPE => Some(GenericConstraintJoinOp::OR),
                TOKEN::SUM => Some(GenericConstraintJoinOp::AND),
                _ => None
            };
            let is_right_op_none = right_op.is_none();
            branch_elements.push(Either::Right(TypeContextExpression {
                static_type,
                static_value,
                if_expr,
                right_condition_op: right_op,
            }));
            if is_right_op_none {
                if until_unmatched.contains(&closing_or_op.token) {
                    break
                } else {
                    return Err(EXPECTED_COMMA_OR_CLOSING(span, Some(closing_or_op.token.clone()), until_unmatched))
                }
            }
            continue
        }

    }
    Ok(GenericConstraintBranch {
        elements: branch_elements,
        right_join_op: None
    })
}