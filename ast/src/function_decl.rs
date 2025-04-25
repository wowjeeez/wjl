use common::span::Span;
use crate::decorator::DecoratorApplication;
use crate::expressions::Expression;
use crate::generic_args::GenericArg;
use crate::lexical_scope::LexicalScope;
use crate::model::Visibility;

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Argument {
    expression: Span<Expression>,
    ty: Option<Span<Expression>>
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Modifier {
    ASYNC,
    GET,
    SET
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionDeclaration {
    name: Span<Expression>,
    visibility: Option<Span<Visibility>>,
    arguments: Vec<Span<Argument>>,
    body: Span<LexicalScope>,
    return_type: Option<Span<Expression>>,
    decorators: Vec<Span<DecoratorApplication>>,
    generics: Vec<Span<GenericArg>>,
    modifiers: Vec<Span<Modifier>>,
    
}