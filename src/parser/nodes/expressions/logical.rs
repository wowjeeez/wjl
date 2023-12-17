use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::lexer::TOKEN::TRUE;
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::EXPECTED_ONEOF;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::expressions::expression::GeneralizedExprAstNode;


#[derive(Clone, Debug, PartialEq)]
pub enum LogicalOperator {
    MUL,
    DIV,
    SUM,
    SUBTRACT,
    MOD,
    B_INV,
    B_XOR,
    L_GTE,
    R_GTE,
    OR,
    AND,
    L_GT,
    R_GT,
    B_SIGNED_RIGHT,
    B_RIGHT_FILL,
    B_AND,
    B_LEFT_FILL,
}

#[derive(Clone, Debug, PartialEq)]
pub struct LogicalExpression {
    left: PartialAstIr, //IDENTIFIER, static value or some expr
    right_op: Option<LogicalOperator>,
}
// parenthesis here are elided into LogicalExpression
pub struct LogicalExpressionAstNode(Vec<LogicalExpression>);

impl AstNodeParsable for LogicalExpressionAstNode {
    // iter should be on a math operator, start should be the left part of the expr
    fn parse(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        let right = iter.next_skip_whitespace();
        

        Ok(PartialAstIr::_NONCE)
    }
}