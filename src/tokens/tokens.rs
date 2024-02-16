use crate::tokens::span::Span;
use either::Either;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum IdentKind {
    DEFAULT(String),
    BACKTICK(String),
}

impl IdentKind {
    pub fn get_text(&self) -> String {
        match self {
            IdentKind::DEFAULT(i) => i.clone(),
            IdentKind::BACKTICK(i) => i.clone()
        }
    }

    pub fn is_btick(&self) -> bool {
        match self {
            IdentKind::DEFAULT(_) => false,
            IdentKind::BACKTICK(_) => true,
        }
    }
}

pub type Literal = Vec<Either<String, Vec<Span<Token>>>>;
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
#[allow(non_camel_case_types, unused)]
pub enum Token {
    ANGLE_LEFT,
    ANGLE_RIGHT,
    BRACE_LEFT,
    BRACE_RIGHT,
    PAREN_LEFT,
    PAREN_RIGHT,
    BRACKET_LEFT,
    BRACKET_RIGHT,
    LEFT_GTE,
    RIGHT_GTE,
    EQ,
    N_EQ,
    COLON,
    SUBTRACT,
    SUM,
    MUL,
    DIV,
    MOD,
    BIT_AND,
    AND,
    OR,
    BIT_XOR,
    BIT_NOT,
    BIT_ZERO_FILL_LEFT_SHIFT,
    BIT_S_RIGHT_SHIFT,
    BIT_ZERO_FILL_RIGHT_SHIFT,
    HASH,
    AT,
    QMARK,
    EXCL_MARK,
    BACKSLASH,
    COMMA,
    DOLLAR,
    SEMI_COLON,
    LINE_BREAK,
    PIPE,
    PERIOD,
    DOUBLE_COLON,
    KEYWORD_VAR,
    KEYWORD_VAL,
    KEYWORD_CONST,
    MOD_KEYWORD_ONCE,
    MOD_KEYWORD_PUBLIC,
    MOD_KEYWORD_PROTECTED,
    MOD_KEYWORD_INTERNAL,
    MOD_KEYWORD_PRIVATE,
    KEYWORD_TRUE,
    KEYWORD_FALSE,
    KEYWORD_NULL,
    KEYWORD_FUNC,
    KEYWORD_CLASS,
    KEYWORD_IMPL,
    KEYWORD_FOR,
    KEYWORD_RETURN,
    KEYWORD_BREAK,
    KEYWORD_CONTINUE,
    KEYWORD_STRUCT,
    KEYWORD_AWAIT,
    KEYWORD_IN,
    KEYWORD_WHILE,
    KEYWORD_MATCH,
    KEYWORD_ENUM,
    KEYWORD_AS,
    KEYWORD_TRY,
    KEYWORD_CATCH,
    KEYWORD_TYPE,
    KEYWORD_CLASSDEF,
    KEYWORD_FUNCDEF,
    KEYWORD_USE,
    KEYWORD_EXT,
    KEYWORD_OPERATOR,
    KEYWORD_DECORATOR,
    KEYWORD_REFLECT,
    KEYWORD_INTERFACE,
    KEYWORD_IF,
    KEYWORD_ELSE,
    KEYWORD_ELSE_IF,
    KEYWORD_YIELD,
    KEYWORD_PURE,
    RANGE_OP,
    INCL_RANGE_OP,
    OP_SPREAD,
    IDENT(IdentKind),
    ASSIGN,
    ASSIGN_SUM,
    ASSIGN_DIV,
    ASSIGN_MOD,
    ASSIGN_MUL,
    ASSIGN_SUB,
    INCR,
    DECR,
    PWR,
    ARROW,
    LITERAL_DOUBLE(Literal),
    LITERAL_SINGLE(Literal),
    WJL_COMPILER_PLACEHOLDER,
    DELIMITER,
    WHITESPACE,
    PIPE_OP,
    FLOAT(String),
    INT(String, bool),
    BINARY_NUMBER(String, bool),
    OCTAL_NUMBER(String, bool),
    HEX_NUMBER(String, bool),
    EXP_NUMBER(String, bool),
    COMMENT(String),
    COMMENT_ML(Vec<String>),
    NONCE,
    OP_ELVIS
}


impl Token {
    pub fn is_math(&self) -> bool {
        match self {
            Token::SUM | Token::SUBTRACT | Token::DIV | Token::MUL | Token::MOD | Token::BIT_NOT | Token::BIT_AND | Token::BIT_XOR | Token::BIT_S_RIGHT_SHIFT | Token::BIT_ZERO_FILL_RIGHT_SHIFT | Token::BIT_ZERO_FILL_LEFT_SHIFT => true,
            _ => false
        }
    }
    pub fn is_binop(&self) -> bool {
        match self {
            Token::OR | Token::AND => true,
            _ => false
        }
    }
}