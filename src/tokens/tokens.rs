use either::Either;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum IdentKind {
    DEFAULT(String),
    BACKTICK(String)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct PathSegment {
    is_bw_colon: Option<bool>, // None is when this is the beginning of the path
    content: IdentKind
}

impl PathSegment {
    pub fn backtick(content: String) -> PathSegment {
        PathSegment {
            is_bw_colon: None,
            content: IdentKind::BACKTICK(content)
        }
    }

    pub fn default(content: String) -> PathSegment {
        PathSegment {
            is_bw_colon: None,
            content: IdentKind::DEFAULT(content)
        }
    }

    pub fn backtick_colon(content: String, colon: bool) -> PathSegment {
        PathSegment {
            is_bw_colon: Some(colon),
            content: IdentKind::BACKTICK(content)
        }
    }

    pub fn default_colon(content: String, colon: bool) -> PathSegment {
        PathSegment {
            is_bw_colon: Some(colon),
            content: IdentKind::DEFAULT(content)
        }
    }
}


impl Token {
    pub fn get_if_is_1_len_ident_no_bt(&self) -> Option<String> {
        if let Token::PATH(inner) = self {
            if inner.len() != 1 {
                return None
            }
            let tok = &inner.first().unwrap().content;
            if let IdentKind::DEFAULT(inner) = tok {
                return Some(inner.clone())
            }
            return None

        }
        return None
    }
}



pub type Literal =  Vec<Either<String, Vec<Token>>>;
#[derive(Clone, Debug, Eq, PartialEq)]
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
    COLON,
    SUBTRACT,
    SUM,
    MUL,
    DIV,
    MOD,
    BIT_AND,
    AND,
    BIT_OR,
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
    SEMI_COLON,
    LINE_BREAK,
    PIPE,
    PERIOD,
    KEYWORD_VAR,
    KEYWORD_VAL,
    KEYWORD_CONST,
    MOD_KEYWORD_ONCE,
    MOD_KEYWORD_PUBLIC,
    MOD_KEYWORD_PROTECTED,
    MOD_KEYWORD_INTERNAL,
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
    KEYWORD_TRY,
    KEYWORD_CATCH,
    KEYWORD_THIS,
    KEYWORD_TYPE,
    KEYWORD_CONSTRUCTOR,
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
    OP_SPREAD,
    PATH(Vec<PathSegment>),
    ASSIGN,
    ASSIGN_SUM,
    ASSIGN_DIV,
    ASSIGN_MOD,
    ASSIGN_MUL,
    INCR_FW,
    INCR_BW,
    DECR_FW,
    DECR_BW,
    PWR,
    ARROW,
    LITERAL_DOUBLE(Literal),
    LITERAL_SINGLE(Literal),
    OP_FUNC_SUM,
    OP_FUNC_DIV,
    OP_FUNC_MOD,
    OP_FUNC_SUB,
    WJL_COMPILER_PLACEHOLDER,
    DELIMITER,
    PIPE_OP,
    NONCE
}