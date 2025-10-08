use common::LocationDataProvider;
use common::span::Span;
use lexer::token::{Token, TokenSpan};
use crate::errors::ParseError;
use crate::TokenIter;

#[derive(Debug)]
pub struct GenericConstraint {

}

#[derive(Debug)]
pub struct GenericArgument {
    variable_name: String,
    constraint: Option<GenericConstraint>
}

pub fn parse_generic_constraint(smart_iterator: &mut TokenIter, var_name: Option<TokenSpan>) -> Result<Span<GenericArgument>, ParseError> {
    let location = smart_iterator.get_location();

    let name = var_name.ok_or(ParseError::ExpectedIdentifier(location, None))?;
    let name = name.clone();
    let location = smart_iterator.get_location();
    let variable_name = name.get();
    let variable_name = variable_name.get_identifier().ok_or(ParseError::ExpectedIdentifier(location, Some(variable_name.clone())))?;

    if smart_iterator.0.peek_is_fn(|x| x.get() == &Token::COLON) {
        //PARSE constraint
        smart_iterator.0.next();
        
    };

    Ok(Span::new(name.start(), name.end(), GenericArgument {
        variable_name,
        constraint: None
    }))
}