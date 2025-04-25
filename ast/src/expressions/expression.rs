use common::span::Span;

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
    ConstantStringLiteral(String, bool),
    MathExpression(LeftRightExpression<MathOperator>),
    LogicalExpression(LeftRightExpression<LogicalOperator>),
    PropertyAccess(Vec<(Span<Expression>, Option<Span<ConnectionType>>)>),
    IndexingAccess(Span<Box<Expression>>, Span<Box<Expression>>),
    FunctionCall(Box<Expression>, Vec<Expression>),
    Await(Box<Expression>),
    Paren(Box<Expression>),
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum ConnectionType {
    PERIOD,
    DBL_COLON
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub struct LeftRightExpression<T> {
    left: Span<Box<Expression>>,
    right: Span<Box<Expression>>,
    operator: Span<T>
}