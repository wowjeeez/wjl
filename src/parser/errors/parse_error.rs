use crate::lexer::{TOKEN, TokenSpan};
use crate::parser::nodes::variable::VARIABLE_DECL_TYPE;

// first enum val is ALWAYS the span that this error originated at
#[allow(non_camel_case_types)]
#[derive(Debug)]
pub enum ParseError {
    // what else did we get instead of the name
    EXPECTED_NAME_OR_DESTRUCTURING(TokenSpan, Option<TOKEN>),
    EXPECTED_NAME_OR_DESTRUCTURING_OR_TYPE(TokenSpan, Option<TOKEN>),
    // modifier, target of the modifier, accepted tokens if len(vec) == 0 nothing will be displayed relating this topic
    MODIFIER_CANT_APPEAR(TokenSpan, TOKEN, TOKEN, Vec<TOKEN>),
    // received value, accepted tokens, same applies as above
    EXPECTED_MODIFIER(TokenSpan, TOKEN, Vec<TOKEN>),

    // what we received, possible closing options
    EXPECTED_COMMA_OR_CLOSING(TokenSpan, Option<TOKEN>, Vec<TOKEN>),
    // what we received instead, what are the possible options
    EXPECTED_ONEOF(TokenSpan, Option<TOKEN>, Vec<TOKEN>),
    // decl type, what we got instead
    NEEDS_INIT(TokenSpan, VARIABLE_DECL_TYPE, Option<TOKEN>),

    VISIBILITY_MODIFIER_SHOULD_BE_FIRST(TokenSpan),

    ONLY_ONE_VISIBILITY_MODIFIER(TokenSpan),
    GENERIC_CANNOT_BE_EMPTY(TokenSpan),
    GE_CONDITION_REQUIRED(TokenSpan),
    GE_ELSE_REQUIRED(TokenSpan),
    GE_BODY_REQUIRED(TokenSpan),
    EXPECTED_EXPRESSION(TokenSpan, Option<TOKEN>)
}