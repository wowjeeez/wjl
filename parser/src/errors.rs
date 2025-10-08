use thiserror::Error;
use common::Location;
use lexer::token::{Token, TokenSpan};



#[derive(Error, Debug)]
pub enum ParseError {
    #[error("expected identifier but got {1:?} instead")]
    ExpectedIdentifier(Location<Token>, Option<Token>),

    #[error("expected type but got {1:?} instead")]
    ExpectedType(Location<Token>, Option<Token>),

    #[error("expected closing {1:?} or comma but got {2:?} instead")]
    ExpectedClosureOrComma(Location<Token>, Token, Option<Token>),

    #[error("expected identifier or literal expression but got {1:?} instead")]
    ExpectedIdentifierOrLiteralExpression(Location<Token>, Option<Token>),

    #[error("expected closing {1:?} but got {2:?} instead")]
    ExpectedClosure(Location<Token>, Token, Option<Token>),
}