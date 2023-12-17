use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::ast::PartialAstIr::{GENERIC_ARG, IDENT_REF};
use crate::parser::errors::ParseError;
use crate::parser::iter::TokenParserIter;
use crate::iter_util::CommonIter;
use crate::parser::nodes::generic_arg::GenericArgAstNode;

#[derive(PartialEq, Debug, Clone)]
pub struct IdentifierAstNode {
    pub(crate) name: String,
    pub(crate) is_backtick: bool,
    pub(crate) prefix_period: bool, //if .<ident>
    pub(crate) prefix_path_seg: bool, //if ::<ident>
    // can only be GENERIC_ARG
    pub(crate) generic_arg: Option<PartialAstIr>,
    pub(crate) is_optional_chain: bool
}

// todo! generics should be denoted like this: Identifier::<generic>
//todo! parse this@something scoping
impl AstNodeParsable for IdentifierAstNode {
    //assuming that the iterator is currently on the initial identifier and this func does take parsing generics (args, not decl) into account. If you want to ignore generics, use `::parse_no_generic_args()`
    fn parse(iter: &mut TokenParserIter, _: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let start = iter.peek_n(0).unwrap();
        let mut parts = vec![];
        let mut ident = IdentifierAstNode {
            name: start.token.get_ident_name(),
            is_backtick: start.token.is_btick_identifier(),
            prefix_period: false,
            prefix_path_seg: false,
            generic_arg: None,
            is_optional_chain: false //TODO!
        };
        let maybe_path_op = iter.peek_skip_whitespace();
        let maybe_generic = iter.peek_n_skip_whitespace(2);
        ident.generic_arg = if maybe_generic.is_some() && maybe_path_op.is_some() {
            let maybe_path_op = maybe_path_op.unwrap();
            let maybe_generic = maybe_generic.unwrap();
            if maybe_path_op.token == TOKEN::S2_PATH_SEGMENT && maybe_generic.token == TOKEN::LEFT_ANGLE {
                //we got a generic boys
                iter.next_skip_whitespace(); //drop path seg
                iter.next_skip_whitespace(); //seek to <
                let generic = GenericArgAstNode::parse(iter, Either::Left(maybe_generic))?;
                Some(generic)
            } else {
                None
            }
        } else {None};

        parts.push(ident);
        while let Some(span) = iter.next() { //we dont consume whitespace here so `<identifier> <identifier>` are parsed as 2 separates
            if span.token.is_btick_or_reg_ident() {
                let maybe_path_op = iter.peek_skip_whitespace();
                let maybe_generic = iter.peek_n_skip_whitespace(2);
                let generic = if maybe_generic.is_some() && maybe_path_op.is_some() {
                    let maybe_path_op = maybe_path_op.unwrap();
                    let maybe_generic = maybe_generic.unwrap();
                    if maybe_path_op.token == TOKEN::S2_PATH_SEGMENT && maybe_generic.token == TOKEN::LEFT_ANGLE {
                        //we got a generic boys
                        iter.next_skip_whitespace(); //drop path seg
                        iter.next_skip_whitespace(); //seek to <
                        let generic = GenericArgAstNode::parse(iter, Either::Left(maybe_generic))?;
                        Some(generic)
                    } else {
                        None
                    }
                } else {None};

                let prev = iter.peek_prev_skip_whitespace(); //this whitespace skip allows for <ident> . <ident> decls
                if prev.is_none() {
                    parts.push(IdentifierAstNode {
                        name: span.token.get_ident_name(),
                        is_backtick: span.token.is_btick_identifier(),
                        prefix_period: false,
                        prefix_path_seg: false,
                        generic_arg: generic,
                        is_optional_chain //TODO!
                    });
                    continue
                }
                let prev = prev.unwrap();
                if prev.token == TOKEN::PERIOD || prev.token == TOKEN::S2_PATH_SEGMENT {
                    parts.push(IdentifierAstNode {
                        name: span.token.get_ident_name(),
                        is_backtick: span.token.is_btick_identifier(),
                        prefix_period: prev.token == TOKEN::PERIOD,
                        prefix_path_seg: prev.token == TOKEN::S2_PATH_SEGMENT,
                        generic_arg: generic,
                        is_optional_chain: false //TODO!
                    });
                    continue
                }
                iter.seek(-1);
                break
            }
            if span.token == TOKEN::PERIOD || span.token == TOKEN::S2_PATH_SEGMENT || span.token == TOKEN::WHITESPACE { //we check for whitespace here so in case that someone went with the cursed syntax of `<ident> <period or ::> <ident>` it still gets parsed as full
                continue
            }
            iter.seek(-1);
            break
        }
        Ok(IDENT_REF(parts))
    }
}