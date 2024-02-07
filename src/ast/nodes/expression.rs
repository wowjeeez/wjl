use crate::ast::nodes::statics::StaticExpr;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum Expression {
    STATIC(StaticExpr),
    PIPE(Vec<Expression>),
    FUNC_CALL(FunctionCallExpr),
    IF(IfExpr),
    MATCH(MatchExpr)
}

#[derive(Clone, Debug)]
pub struct FunctionCallExpr {}

#[derive(Clone, Debug)]
pub struct IfExpr {}

#[derive(Clone, Debug)]
pub struct MatchExpr {}

#[derive(Clone, Debug)]
pub struct LogExpr {}