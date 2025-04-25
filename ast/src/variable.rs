use common::span::Span;
use crate::expressions::Expression;
use crate::model::Visibility;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum VariableType {
    CONSTANT,
    ONCE,
    MUTABLE
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct VariableDeclaration {
    visibility: Span<Visibility>,
    name: Span<Expression>,
    ty: Option<Span<Expression>>,
    mutability: Span<VariableType>,
    initializer: Option<Span<Expression>>,
}