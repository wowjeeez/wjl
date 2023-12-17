use either::Either;
use crate::lexer::TokenSpan;
use crate::parser::ast::{PartialAstIr, AstNodeParsable};
use crate::parser::errors::ParseError;
use crate::parser::iter::TokenParserIter;

pub struct CodeBlockAstNode {
    content: Vec<PartialAstIr>,
    label: Option<String>,
    is_async: bool
}

impl AstNodeParsable for CodeBlockAstNode {
    // assuming that iter is currently on {
    fn parse(iter: &mut TokenParserIter, _: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        while let span = iter.next_skip_whitespace() {

        }
        Ok(PartialAstIr::_NONCE)


    }
}