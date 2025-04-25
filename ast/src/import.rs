use common::span::Span;
use crate::expressions::Expression;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Import {
    is_native: bool,
    expr: Span<Expression>
}