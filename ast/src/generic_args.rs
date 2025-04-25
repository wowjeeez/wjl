use common::span::Span;
use crate::expressions::Expression;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct GenericArg {
    name: String,
    constraint: Option<Span<Expression>>,
}