use either::Either;
use crate::lexer::TokenSpan;
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::errors::ParseError;
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::identifier::IdentifierAstNode;

#[derive(Clone, Debug, PartialEq)]
pub struct FuncCallAstNode {
    function_name: Vec<IdentifierAstNode>,
    arguments: Vec<PartialAstIr>,
    parenthesis: bool,
    is_operator: bool
}
//TODO! await keyword should be a parent of this node (if present)

impl AstNodeParsable for FuncCallAstNode {
    // parsing rules, this func should be called when we dont really know what format a function is in:
    /*
        <static value or identifier or expr> <identifier> <static value or identifier or expr> -> operator func call (start should be on the first expr/ident/static value as an AST node or token if static value)
        <identifier> <static value or identifier or expr>* -> no paren func call, expression are parsed until newline or unmatched ) (start should be on the starting ident as an AST)
        <identifier>(<args>*) -> usual func call (starting should be on the starting ident as an AST)
     */
    fn parse(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {
        todo!()
    }
}

impl FuncCallAstNode {
    pub fn parse_operator(iter: &mut TokenParserIter, left_arg: Either<TokenSpan, PartialAstIr>) -> Result<PartialAstIr, ParseError> {

    }

    pub fn parse_parenless(iter: &mut TokenParserIter, name: PartialAstIr) -> Result<PartialAstIr, ParseError> {

    }

    pub fn parse_paren(iter: &mut TokenParserIter, name: PartialAstIr) -> Result<PartialAstIr, ParseError> {

    }
}