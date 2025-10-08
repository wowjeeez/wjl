use thiserror::Error;
use common::Location;
use lexer::token::{Token, TokenSpan};



#[derive(Error, Debug)]
pub enum ParseError {
    #[error("expected identifier but got {0:?} instead")]
    ExpectedIdentifier(Location<Token>, Option<Token>),

    #[error("expected type but got {0:?} instead")]
    ExpectedType(Location<Token>, Option<Token>)
}