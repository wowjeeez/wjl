use common::span::Span;
use crate::expressions::Expression;

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum Statement {
    Assignment {
        target: Span<Expression>,
        value: Box<Expression>,
        assignment_type: Span<AssignmentType>,
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum AssignmentType {
    ADD,
    SUB,
    MUL,
    DIV,
    EXP,
    EQ,
    MOD,
    LOGI_AND,
    LOGO_OR,
    L_SHIFT,
    R_SHIFT,
    U_R_SHIFT,
    B_OR,
    B_XOR,
    B_AND,
    NSC
}