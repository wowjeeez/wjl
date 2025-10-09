mod structures;
mod errors;
mod ast;

use common::iter::SmartIterator;
use common::{Location, LocationDataProvider};
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::ast::AstNode;
use crate::errors::ParseError;
use crate::structures::function::{parse_async_func_decl, parse_func_decl, parse_func_signature};
use crate::structures::lexical_expr::parse_expr;

pub struct TokenIter (SmartIterator<TokenSpan>);

impl LocationDataProvider<Token> for TokenIter {
    fn get_location(&self) -> Location<Token> {
        Location::wrap(self.0.curr().unwrap_or(&Span::zeroed()).clone())
    }
}

impl TokenIter {
    pub fn ensure_then_next(&mut self, rhs: Token) -> Option<TokenSpan> {
        if self.0.peek_is_fn(|x| x.get() == &rhs) {
            self.0.next();
        }
        None
    }

    pub fn peek_and_next_if_fn(&mut self, predicate: impl Fn(&Token) -> bool) -> Option<TokenSpan> {
        if self.0.peek_is_fn(|x| predicate(x.get())) {
            self.0.next();
        }
        None
    }

    pub fn next_or_expect_expr(&mut self) -> Result<TokenSpan, ParseError> {
        let location = self.get_location();
        let (next, _) = self.0.next().ok_or(ParseError::ExpectedExpression(location, None))?;
        return Ok(next.clone())
    }

    pub fn next_expect_token(&mut self, expected: Token) -> Result<TokenSpan, ParseError>  {
        let location = self.get_location();
        let (next, _) = self.0.next().ok_or(ParseError::ExpectedToken(location, expected.clone(), None))?;
        let next = next.clone();
        let location = self.get_location();
        if next.get() != &expected {
            return Err(ParseError::ExpectedToken(location, expected.clone(), Some(next.get().clone())))
        }
        Ok(next.clone())
    }

    pub fn next_expect_closure_or_comma(&mut self, expected: Token) -> Result<TokenSpan, ParseError>  {
        let location = self.get_location();
        let (next, _) = self.0.next().ok_or(ParseError::ExpectedClosureOrComma(location, expected.clone(), None))?;
        let next = next.clone();
        let location = self.get_location();
        if next.get() != &expected {
            return Err(ParseError::ExpectedClosureOrComma(location, expected.clone(), Some(next.get().clone())))
        }
        Ok(next.clone())
    }

}

pub fn parse_block(smart_iterator: &mut TokenIter) -> Result<Vec<AstNode>, ParseError> {
    let mut parse_tree = vec![];
    loop {
        let location  = smart_iterator.get_location();
        let next_token = smart_iterator.0.peek();
        if next_token.is_none() {
            break
        }
        let next_token = next_token.unwrap();
        if next_token.get() == &Token::R_CURLY {
            break
        }
        let node = match next_token.get() {
            Token::IDENT(_) | Token::D_LITERAL(_) |
                Token::S_LITERAL(_) |
            Token::D_NUMBER(_) |
                Token::B_NUMBER(_) |
            Token::H_NUMBER(_) |
                Token::O_NUMBER(_) |
            Token::FD_NUMBER(_) |
                Token::KEYWORD_TRUE |
            Token::KEYWORD_FALSE => {
                let expr = parse_expr(smart_iterator)?;
                AstNode::Expression(expr)
            }
            Token::KEYWORD_FUNCTION => {
                let func = parse_func_decl(smart_iterator)?;
            }
            Token::KEYWORD_IF => todo!(),
            Token::KEYWORD_CLASS => todo!(),
            Token::KEYWORD_WHILE => todo!(),
            Token::KEYWORD_FOR => todo!(),
            Token::KEYWORD_MATCH => todo!(),
            Token::KEYWORD_ASYNC => {
                let func = parse_async_func_decl(smart_iterator)?;
            }
            Token::KEYWORD_CONSTRUCTOR => todo!(),
            Token::KEYWORD_DESTRUCTOR => todo!(),
            Token::KEYWORD_BREAK => todo!(),
            Token::KEYWORD_CONTINUE => todo!(),
            Token::KEYWORD_NEW => todo!(),
            Token::KEYWORD_TRY => todo!(),
            Token::KEYWORD_CATCH => todo!(),
            Token::KEYWORD_FINALLY => todo!(),
            Token::KEYWORD_ENTER => todo!(),
            Token::KEYWORD_AWAIT => {
                let expr = parse_expr(smart_iterator)?;
                AstNode::Expression(expr)
            }
            Token::KEYWORD_RETURN => todo!(),
            Token::KEYWORD_YIELD => todo!(),
            Token::KEYWORD_INTERFACE => todo!(),
            Token::KEYWORD_PUBLIC => todo!(),
            Token::KEYWORD_PRIVATE => todo!(),
            Token::KEYWORD_PROTECTED => todo!(),
            Token::KEYWORD_REFLECTION => todo!(),
            Token::KEYWORD_VAL => todo!(),
            Token::KEYWORD_VAR => todo!(),
            Token::KEYWORD_ONCE => todo!(),
            Token::KEYWORD_USE => todo!(),
            Token::KEYWORD_NATIVE => todo!(),
            Token::KEYWORD_DEFINE => todo!(),
            Token::WHITESPACE_MERGED => todo!(),
            Token::KEYWORD_NULL => {
                let expr = parse_expr(smart_iterator)?;
                AstNode::Expression(expr)
            }
            Token::TILDE => todo!(),
            _ => return Err(ParseError::UnexpectedToken(location))
        };
        parse_tree.push(node);
    }
    return Ok(parse_tree)
}