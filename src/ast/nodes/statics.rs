use crate::ast::nodes::expression::Expression;

#[derive(Clone, Debug)]
#[allow(non_camel_case_types)]
pub enum StaticExpr {
    STRING_SINGLE(StringTemplate),
    STRING_DOUBLE(StringTemplate),
    FLOAT(String),
    INT(String, bool),
    HEX(String, bool),
    EXP(String, bool),
    BIN(String, bool),
    OCT(String, bool),
    ARR(Vec<Expression>)
}
#[derive(Debug, Clone)]
struct StringTemplate {
    raw: Vec<Option<String>>,
    expressions: Vec<Option<Expression>>
}