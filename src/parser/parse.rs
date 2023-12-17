use std::vec;
use either::Either;
use crate::iter_util::CommonIter;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{PartialAstIr, AstNodeParsable};
use crate::parser::ast::PartialAstIr::{ARBIT_BLOCK, FLOAT, IDENT_REF, INT, LOGICAL_OP, PAREN_EXPR};
use crate::parser::errors::ParseError;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::array::ArrayAstNode;
use crate::parser::nodes::generic_decl::GenericDeclarationAstNode;
use crate::parser::nodes::identifier::IdentifierAstNode;
use crate::parser::nodes::variable::VariableAstNode;

pub trait ParsableTokenStream {
    fn parse_into_ast_vec(self: Self, stop_on_unmatched: Vec<TOKEN>) -> Result<(Vec<PartialAstIr>, usize), ParseError>;
}


impl ParsableTokenStream for Vec<TokenSpan> {
    fn parse_into_ast_vec(self: Vec<TokenSpan>, stop_on_unmatched: Vec<TOKEN>) -> Result<(Vec<PartialAstIr>, usize), ParseError> {
        let mut iter = TokenParserIter::wrap(self);
        let mut dumb_token_tree = vec![];
        while let Some(span) = iter.next() {
            if stop_on_unmatched.contains(&span.token) { //we make the probably safe assumption here that the consumer will request something that does not denote the beginning of something
                break
            }
            let mut nodes = if span.is_variable_declaration() {
                vec![VariableAstNode::parse(&mut iter, Either::Left(span))?]
            } else if span.token == TOKEN::LEFT_PAREN {
                iter.next(); //slide to token after (
                let rem = iter.remaining_inclusive();
                let (chunk, seek_to) = rem.parse_into_ast_vec(vec![TOKEN::RIGHT_PAREN])?;
                iter.seek((seek_to as isize) + 1);
                let node = PAREN_EXPR(chunk);
                vec![node]
            } else if span.token == TOKEN::LEFT_BRACE {
                iter.next(); //slide to token after {
                let rem = iter.remaining_inclusive();
                let (chunk, seek_to) = rem.parse_into_ast_vec(vec![TOKEN::RIGHT_BRACE])?;
                iter.seek((seek_to as isize) + 1);
                vec![ARBIT_BLOCK(chunk)]
            } else if span.token == TOKEN::LEFT_BRACKET {
                vec![ArrayAstNode::parse(&mut iter, Either::Left(span))?]
            } else if span.token.is_btick_or_reg_ident() {
                vec![IdentifierAstNode::parse(&mut iter, Either::Left(span))?]
            } else if span.token.is_s2_num() {
                vec![match span.token {
                    TOKEN::S2_FLOAT_LITERAL(f) => FLOAT(f),
                    TOKEN::S2_INT_LITERAL(i) => INT(i),
                    _ => unreachable!()
                }]
            } else if span.token == TOKEN::LEFT_ANGLE { //TODO! fix this because it might be a math operator
                vec![GenericDeclarationAstNode::parse(&mut iter, Either::Left(span))?]
            } else {
                vec![]
            };
            if nodes.len() != 0 {
                dumb_token_tree.append(&mut nodes);
            }
        }
        Ok((dumb_token_tree, iter.pos()))
    }
}

pub fn parse_token_stream(stream: Vec<TokenSpan>) -> Result<Vec<PartialAstIr>, ParseError> {
    stream.parse_into_ast_vec(vec![]).map(|(x, _)| x)
}