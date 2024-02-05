use crate::ast::nodes::variable::{Assignment, VariableDeclaration};
use crate::errors::{ErrorReporter, WjlError};
use crate::iter::{GenericIterator, PeekableIterator, wrap_iter};
use crate::tokens::span::Span as GenericSpan;
use crate::tokens::Token;
use crate::tokens::Token::{KEYWORD_CONST, KEYWORD_VAL, KEYWORD_VAR, MOD_KEYWORD_ONCE};

pub type TokenSpan = GenericSpan<Token>;

#[derive(Debug, Clone)]
#[allow(non_camel_case_types, unused)]
pub enum Ast {
    VAR_DECL(VariableDeclaration),
    ASSIGNMENT(Assignment)
}

trait ToAst {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast>;
}

impl PeekableIterator<TokenSpan> {
    pub fn next_if(&mut self, eq_tok: Token) -> Option<TokenSpan> {
        self.next().map(|x| if x.get_inner() == eq_tok {
            Some(x)
        } else {None}).flatten()
    }

}

impl ToAst for Vec<TokenSpan> {
    fn into_ast(self, reporter: &mut ErrorReporter) -> Vec<Ast> {
        let mut iter = wrap_iter(self);
        let mut stream = vec![];
        while let Some(span) = iter.next() {
            let tok = span.get_inner();
            if [KEYWORD_VAL, KEYWORD_VAR, KEYWORD_CONST].contains(&tok) {
                let prev = iter.peek_prev();
                let decl_keyword_idx = iter.get_index().unwrap();
                let next = iter.next();
                if next.is_none() {
                    reporter.add(WjlError::char(span.end).message("Expected variable name or destructuring expression, got nothing.").ok());
                    break
                }
                let next = next.unwrap();

                let var_name = if let Token::IDENT(ident) = next.get_inner() {

                } else if next.get_inner() == Token::BRACE_LEFT {

                } else if next.get_inner() == Token::BRACKET_LEFT {

                } else if next.get_inner() == Token::PAREN_LEFT {

                } else {
                    reporter.add(WjlError::char(next.start).message(format!("Expected variable name or destructuring expression, got {}.", next.to_colored_str())).ok());
                    break
                };
            }
        }
        stream
    }
}