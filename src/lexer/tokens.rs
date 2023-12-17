use crate::lexer::span::TokenSpan;


#[derive(Debug, PartialEq, Clone)]
pub struct DestructureExpressionElementNode {
    pub(crate) key: String,
    pub(crate) alias: Option<String>,
    pub(crate) default: Option<Vec<TokenSpan>>,
    pub(crate) is_object: bool,
    pub(crate) actual_binding: bool,
    pub(crate) children: Option<Vec<DestructureExpressionElementNode>>
}

#[derive(Debug, PartialEq, Clone)]
#[allow(non_camel_case_types)]
pub enum TOKEN {
    DIRECTIVE_DEP,
    DIRECTIVE_EXTERNAL_DEP,
    KEYWORD_UNSAFE,
    ACCESSMOD_EXTERNAL,
    ACCESSMOD_PRIVATE,
    ACCESSMOD_INTERNAL,
    DECL_VAL,
    TRUE,
    FALSE,
    DECL_VAR,
    DECL_ONCE,
    DECL_CONST,
    KEYWORD_EXACT,
    KEYWORD_TYPEDEF,
    KEYWORD_TYPE_THEN,
    KEYWORD_TYPE_END,
    LEFT_BRACKET,
    RIGHT_BRACKET,
    LEFT_PAREN,
    RIGHT_PAREN,
    LEFT_BRACE,
    RIGHT_BRACE,
    EQ,
    EXCL_MARK,
    QUESTION_MARK,
    SEMI_COLON,
    COLON,
    PERIOD,
    SUBTRACT,
    DIV,
    MUL,
    B_XOR,
    B_INV,
    SUM,
    COMMA,
    MOD,
    NUMBER(u8),
    IDENTIFIER(String),
    STRING_LITERAL(String),
    BACKTICK_LITERAL(String),
    COMMENT(String),
    COMMENT_MLINE(Vec<String>),
    LEFT_ANGLE,
    RIGHT_ANGLE,
    KEYWORD_TYPELAYER,
    KEYWORD_IF,
    KEYWORD_ELSE,
    KEYWORD_MATCH,
    KEYWORD_STRUCT,
    KEYWORD_INTERFACE,
    BACKSLASH,
    PIPE,
    AND,
    KEYWORD_RETURN,
    KEYWORD_TRY,
    KEYWORD_UNWIND,
    KEYWORD_CATCH,
    KEYWORD_CLASS,
    KEYWORD_PURE,
    KEYWORD_ENUM,
    KEYWORD_RAISE,
    KEYWORD_FUNC,
    KEYWORD_ASYNC,
    KEYWORD_BREAK,
    KEYWORD_WHILE,
    KEYWORD_FOR,
    KEYWORD_CONTINUE,
    KEYWORD_YIELD,
    KEYWORD_ABSTRACT,
    KEYWORD_FINAL,
    AT,
    MAP_OPERATOR,
    KEYWORD_OPERATOR,
    KEYWORD_STATIC,
    KEYWORD_IN,
    KEYWORD_IS,
    KEYWORD_DECORATOR,
    KEYWORD_NULL,
    SPACE,
    DOLLAR,
    WHITESPACE,
    NEWLINE,
    EOF,
    UNDERLINE, //only captured for the sake of better numeric readability is not reserved in names etc
    _NONCE,

    S2_INT_LITERAL(i64), //S2_ prefix stands for stage 2 parsing
    S2_FLOAT_LITERAL(f64),
    S2_IMPORT_PATH(Vec<TokenSpan>),
    S2_EXTERNAL_IMPORT {
        module: String,
        untyped: bool,
        receiver: Box<TokenSpan>
    },
    S2_REASS_ADD,
    S2_REASS_SUB,
    S2_INCR,
    S2_DECR,
    S2_EQ,
    S2_ASSIGN,
    S2_REASS_MUL,
    S2_REASS_DIV,
    S2_ELVIS,
    S2_LEVIS,
    S2_L_GTE,
    S2_R_GTE,
    S2_AND,
    S2_POW,
    S2_B_AND,
    S2_B_RIGHT_FILL,
    S2_B_LEFT_FILL,
    S2_B_SIGNED_RIGHT,
    S2_PIPE_OP,
    S2_PATH_SEGMENT,
    S2_OR,
    S2_ARBIT_BLOCK(Vec<TokenSpan>),
    S2_LABEL(String),
    ERR_PLACEHOLDER_LOG_EXPR,
    ERR_PLACEHOLDER_TYPE_EXPR,
    S2_SPREAD_OP
}

impl TOKEN {
    pub fn is_identifier(&self) -> bool {
        match self {
            TOKEN::IDENTIFIER(_) => true,
            _ => false
        }
    }

    pub fn is_btick_identifier(&self) -> bool {
        match self {
            TOKEN::BACKTICK_LITERAL(_) => true,
            _ => false
        }
    }

    pub fn is_btick_or_reg_ident(&self) -> bool {
        self.is_btick_identifier() || self.is_identifier()
    }

    pub fn is_static_value(&self) -> bool {
        match self {
            TOKEN::STRING_LITERAL(_) | TOKEN::S2_INT_LITERAL(_) | TOKEN::S2_FLOAT_LITERAL(_) | TOKEN::TRUE | TOKEN::FALSE | TOKEN::KEYWORD_NULL => true,
            _ => false
        }
    }

    pub fn is_label(&self) -> bool {
        match self {
            TOKEN::S2_LABEL(_) => true,
            _ => false
        }
    }

    pub fn get_ident_name(&self) -> String {
        match self {
            TOKEN::IDENTIFIER(name) => name.clone(),
            TOKEN::BACKTICK_LITERAL(name) => name.clone(),
            _ => unreachable!()
        }
    }

    pub fn is_num(&self) -> bool {
        match self {
            TOKEN::NUMBER(_) => true,
            _ => false
        }
    }

    pub fn is_str_literal(&self) -> bool {
        match self {
            TOKEN::STRING_LITERAL(_) => true,
            _ => false
        }
    }


    pub fn get_num(&self) -> u8 {
        match self {
            TOKEN::NUMBER(n) => n.clone(),
            _ => unreachable!()
        }
    }

    pub fn is_s2_num(&self) -> bool {
        match self {
            TOKEN::S2_FLOAT_LITERAL(_) | TOKEN::S2_INT_LITERAL(_) => true,
            _ => false
        }
    }
}