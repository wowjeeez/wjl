mod structures;
mod errors;

use common::iter::SmartIterator;
use common::{Location, LocationDataProvider};
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::structures::function::parse_func_decl;

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

pub fn to_parse_tree(token_stream: Vec<Span<Token>>) -> Result<(), ParseError> {
    let iter = SmartIterator::new(token_stream);
    let mut token_iter = TokenIter(iter);
    let root = token_iter.0.next();
    if root.is_none() {

    }
    let (root, _) = root.unwrap();
    let root = root.clone();
    let result = match root.get() {
        Token::L_PAREN => {}
        Token::R_PAREN => {}
        Token::L_CURLY => {}
        Token::R_CURLY => {}
        Token::L_BRACKET => {}
        Token::R_BRACKET => {}
        Token::L_ANGLE => {}
        Token::R_ANGLE => {}
        Token::ASSIGN => {}
        Token::EXCL => {}
        Token::QMARK => {}
        Token::SUM => {}
        Token::SUB => {}
        Token::DIV => {}
        Token::MUL => {}
        Token::MOD => {}
        Token::COLON => {}
        Token::PIPE => {}
        Token::AMP => {}
        Token::AT => {}
        Token::SEMICOLON => {}
        Token::PERIOD => {}
        Token::HASH => {}
        Token::IDENT(_) => {}
        Token::D_LITERAL(_, _) => {}
        Token::S_LITERAL(_, _) => {}
        Token::D_NUMBER(_) => {}
        Token::B_NUMBER(_) => {}
        Token::H_NUMBER(_) => {}
        Token::O_NUMBER(_) => {}
        Token::FD_NUMBER(_) => {}
        Token::KEYWORD_TRUE => {}
        Token::KEYWORD_FALSE => {}
        Token::KEYWORD_FUNCTION => parse_func_decl(&mut token_iter, root.clone())?,
        Token::KEYWORD_IF => {}
        Token::KEYWORD_ELSE => {}
        Token::KEYWORD_ELSEIF => {}
        Token::KEYWORD_CLASS => {}
        Token::KEYWORD_WHILE => {}
        Token::KEYWORD_FOR => {}
        Token::KEYWORD_MATCH => {}
        Token::KEYWORD_ASYNC => {}
        Token::KEYWORD_CONSTRUCTOR => {}
        Token::KEYWORD_DESTRUCTOR => {}
        Token::KEYWORD_BREAK => {}
        Token::KEYWORD_CONTINUE => {}
        Token::KEYWORD_NEW => {}
        Token::KEYWORD_TRY => {}
        Token::KEYWORD_CATCH => {}
        Token::KEYWORD_FINALLY => {}
        Token::KEYWORD_ENTER => {}
        Token::KEYWORD_AWAIT => {}
        Token::KEYWORD_RETURN => {}
        Token::KEYWORD_YIELD => {}
        Token::KEYWORD_INTERFACE => {}
        Token::KEYWORD_PUBLIC => {}
        Token::KEYWORD_PRIVATE => {}
        Token::KEYWORD_PROTECTED => {}
        Token::KEYWORD_REFLECTION => {}
        Token::KEYWORD_VAL => {}
        Token::KEYWORD_VAR => {}
        Token::KEYWORD_ONCE => {}
        Token::KEYWORD_USE => {}
        Token::KEYWORD_NATIVE => {}
        Token::KEYWORD_DEFINE => {}
        Token::WHITESPACE_MERGED => {}
        Token::D_COLON => {}
        Token::KEYWORD_NULL => {},
        Token::COMMA => {},
        Token::TILDE => {}
    };

    Ok(())
}
