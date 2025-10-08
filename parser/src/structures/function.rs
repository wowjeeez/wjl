use common::iter::SmartIterator;
use common::LocationDataProvider;
use lexer::token::TokenSpan;
use crate::errors::ParseError;
use crate::structures::qualified_ident::parse_qualified_ident;
use crate::TokenIter;

pub fn parse_func_decl(smart_iterator: &mut TokenIter, root: TokenSpan) -> Result<(), ParseError> {
    let start_location = smart_iterator.get_location();
    let (name_start, index) = smart_iterator
        .0
        .next()
        .ok_or(ParseError::ExpectedIdentifier(start_location, None))?;
    let name_start = name_start.clone();
    let qualified_name = parse_qualified_ident(smart_iterator, name_start)?;
    dbg!(&qualified_name);
    let args = parse_func_arguments(smart_iterator)?;
    return Ok(())

}

pub fn parse_func_arguments(smart_iterator: &mut TokenIter) -> Result<(), ParseError> {
    Ok(())
}