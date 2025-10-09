use common::span::Span;
use crate::structures::lexical_expr::Expression;

#[derive(Debug, Clone)]
pub enum VariableName {
    Ident(Span<Expression>),
    ObjectDestructuring(),
    ArrayDestructuring(),
}