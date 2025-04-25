use common::span::Span;
use crate::statements::Statement;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum NumberType {
    Float,
    Decimal,
    Binary,
    Octal,
    Hexadecimal,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum MathOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Pow,
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum LogicalOperator {
    And,
    Or,
    Gt,
    Lt,
    Gte,
    Lte,
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Expression {
    Number(String, NumberType),
    Boolean(bool),
    StaticIdent(String),
    ConstantStringLiteral(String, bool),
    MathExpression(LeftRightExpression<MathOperator>),
    LogicalExpression(LeftRightExpression<LogicalOperator>),
    PropertyAccess(Vec<(Span<Expression>, Option<Span<ConnectionType>>)>),
    IndexingAccess(Span<Box<Expression>>, Span<Box<Expression>>),
    FunctionCall(Span<Box<Expression>>, Vec<Span<Expression>>),
    Await(Span<Box<Expression>>),
    Paren(Span<Box<Expression>>),
    ObjectLiteral(Vec<Span<Expression>>),
    AssignmentStatement(Span<Box<Statement>>), // Statement::Assignment ALWAYS
    KeyDefinitionStatement(Span<Box<Expression>>, Span<Box<Expression>>),
    SpreadExpression(Span<Box<Expression>>),
    ArrayLiteral(Vec<Span<Expression>>),

}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConnectionType {
    PERIOD_OPT_CHAIN,
    PERIOD_ASSERT,
    PERIOD,
    DBL_COLON
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LeftRightExpression<T> {
    left: Span<Box<Expression>>,
    right: Span<Box<Expression>>,
    operator: Span<T>
}