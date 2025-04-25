use crate::span::Span;

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Token {
    L_PAREN,
    R_PAREN,
    L_CURLY,
    R_CURLY,
    L_BRACKET,
    R_BRACKET,
    L_ANGLE,
    R_ANGLE,
    EQ,
    EXCL,
    QMARK,
    SUM,
    SUB,
    DIV,
    MUL,
    MOD,
    COLON,
    PIPE,
    AMP,
    SEMICOLON,
    PERIOD,
    HASH,
    IDENT(String),
    D_LITERAL(String, bool),
    S_LITERAL(String, bool),
    D_NUMBER(String),
    B_NUMBER(String),
    H_NUMBER(String),
    O_NUMBER(String),
    FD_NUMBER(String),
    KEYWORD_TRUE,
    KEYWORD_FALSE,
    KEYWORD_FUNCTION,
    KEYWORD_IF,
    KEYWORD_ELSE,
    KEYWORD_ELSEIF,
    KEYWORD_CLASS,
    KEYWORD_WHILE,
    KEYWORD_FOR,
    KEYWORD_MATCH,
    KEYWORD_ASYNC,
    KEYWORD_CONSTRUCTOR,
    KEYWORD_DESTRUCTOR,
    KEYWORD_BREAK,
    KEYWORD_CONTINUE,
    KEYWORD_NEW,
    KEYWORD_TRY,
    KEYWORD_CATCH,
    KEYWORD_FINALLY,
    KEYWORD_ENTER,
    KEYWORD_AWAIT,
    KEYWORD_RETURN,
    KEYWORD_YIELD,
    KEYWORD_INTERFACE,
    KEYWORD_PUBLIC,
    KEYWORD_PRIVATE,
    KEYWORD_PROTECTED,
    KEYWORD_REFLECTION,
    KEYWORD_VAL,
    KEYWORD_VAR,
    KEYWORD_ONCE,
    KEYWORD_THIS,
    KEYWORD_USE,
    KEYWORD_NATIVE,
    KEYWORD_DEFINE,
    WHITESPACE_MERGED
}

pub type TokenSpan = Span<Token>;


impl Token {
    pub fn spanned(self, start: usize, end: usize) -> Span<Token> {
        TokenSpan::new(start, end, self)
    }

    pub fn spanned_one(self, start: usize) -> Span<Token> {
        TokenSpan::new(start, start, self)
    }
}