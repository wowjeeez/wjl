use either::Either;
use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::ast::{AstNodeParsable, PartialAstIr};
use crate::parser::errors::ParseError;
use crate::parser::errors::ParseError::{EXPECTED_COMMA_OR_CLOSING, EXPECTED_EXPRESSION};
use crate::parser::iter::TokenParserIter;
use crate::parser::nodes::expressions::func_call::FuncCallAstNode;

// generic expression parser, should be invoked in places where expressions are expected
pub struct GeneralizedExprAstNode();

impl GeneralizedExprAstNode {
    // basically, this should be invoked when we expect an expression of some sort
    // should be on the beginning of an expression: (, some sort of assignment, or a static value
    // will automatically stop the parsing on expression ending tokens: comma, semicolon, newline, or )
    // does not operate on type ctx expressions, for that use one of the generic_ structs or the ArbitTypeContextAstNode struct
    // this parser is quite smart and can deduce a lot of things, those being:
    // - Code block expressions
    // - If/match expressions
    // - For/while expressions (if I decide to implement it)
    // - Logical expressions (math, bit ops)
    // - Object declarations
    // - Array declarations
    // - Class declarations (expression wise e.g x = class ..)
    // - Map operators
    // - Try catch/unwind expressions
    // - Parenthesis expressions
    // - Function calls (either paren less or non paren less)
    // - Operator func calls
    // - Closure declarations
    // - Pipe expressions
    // Await, yield, break, continue, return exprs
    fn parse<T: Fn(&TokenParserIter) -> bool>(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>, stopper: T) -> Result<PartialAstIr, ParseError> {
        let expr_start = iter.next_skip_whitespace();
        if expr_start.is_none() {
            return Err(EXPECTED_EXPRESSION(start.unwrap_left(), None))
        }
        let expr_start = expr_start.unwrap();
        todo!()
    }
}

impl GeneralizedExprAstNode {
    // will parse something that starts with an identifier or a static value, idents should be provided as an Ast node
    fn parse_ident_or_static_val(iter: &mut TokenParserIter, start: Either<TokenSpan, PartialAstIr>) -> Result<Option<PartialAstIr>, ParseError> {
        let next = iter.next_skip_whitespace();
        if next.is_none() {
            return Ok(None)
        }
        let next = next.unwrap();
        if start.is_right() { //meaning that start is an identifier
            if next.token == TOKEN::LEFT_PAREN {
                //paren func call
                return FuncCallAstNode::parse_paren(iter, start.unwrap_right()).map(|x| Some(x));
            }

            if next.token.is_btick_or_reg_ident() || next.token.is_static_value() {
                // can be operator or parenless (ambigous)
                return FuncCallAstNode::parse(iter, start).map(|x| Some(x))
            }
        }
        // start is static value or not a function

        if start.is_left() && next.token.is_btick_or_reg_ident() {
            // definitely operator
            return FuncCallAstNode::parse_operator(iter, start).map(|x| Some(x))
        }

        //we parsed function calls, no we handle logical exprs, pipe and map exprs
        if next.token.is_logical_op() { //logical expr

        }

        if next.token == TOKEN::MAP_OPERATOR { //map expr

        }

        if next.token == TOKEN::S2_PIPE_OP { //pipe expr

        }




        Ok(None)
    }
}