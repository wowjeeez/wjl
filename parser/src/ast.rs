use common::span::Span;
use crate::structures::lexical_expr::Expression;

#[derive(Debug, Clone)]
pub enum AstNode {
    Expression(Span<Expression>)
}