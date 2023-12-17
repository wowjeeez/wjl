use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{PartialAstIr, AstNodeParsable};
use crate::parser::ast::PartialAstIr::ARRAY_DECL;
use crate::parser::dbg_print::dbg_vec;
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::EXPECTED_COMMA_OR_CLOSING;
use crate::parser::iter::TokenParserIter;
use crate::parser::parse::ParsableTokenStream;

#[derive(PartialEq, Debug, Clone)]
pub struct ArrayAstNode(pub(crate) Vec<PartialAstIr>);

impl AstNodeParsable for ArrayAstNode {
    // assuming that the iter is currently on the opening [
    fn parse(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let mut elements = vec![];
        while let span = iter.next_skip_whitespace() {
            if span.is_none() {
                return Err(EXPECTED_COMMA_OR_CLOSING(start.left().unwrap(), None, vec![TOKEN::RIGHT_BRACKET]))
            }
            let span = span.unwrap();
            if span.token == TOKEN::RIGHT_BRACKET {
                break
            }
            let (mut rem, seek) = iter.remaining_inclusive().parse_into_ast_vec(vec![TOKEN::COMMA, TOKEN::RIGHT_BRACKET])?;
            iter.seek(seek as isize);
            elements.append(&mut rem);
            let curr = iter.peek_n(0).unwrap();
            if curr.token == TOKEN::RIGHT_BRACKET {
                break
            }
        }
        Ok(ARRAY_DECL(ArrayAstNode(elements)))
    }
}