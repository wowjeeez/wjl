mod nodes;
mod lexical_scope;
mod import;
mod expressions;
mod variable;
mod model;
mod function_decl;
mod decorator;
mod generic_args;
mod class_decl;
mod statements;

use common::iter::SmartIterator;
use lexer::token::TokenSpan;

pub fn tokens_to_ast(stream: Vec<TokenSpan>) {
    let mut iter = SmartIterator::new(stream);
    while let Some((token, index)) = iter.next() {
        
    }
}


