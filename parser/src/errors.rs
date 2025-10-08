use thiserror::Error;
use common::Location;
use common::span::Span;
use lexer::token::{Token, TokenSpan};



#[derive(Error, Debug)]
pub enum ParseError {
    #[error("expected identifier but got {0:?} instead")]
    ExpectedIdentifier(Location<Token>, Option<Token>)
}