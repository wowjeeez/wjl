use common::span::Span;

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
    D_COLON,
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
    AT,
    SEMICOLON,
    PERIOD,
    COMMA,
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
    WHITESPACE_MERGED,
    KEYWORD_NULL
}

pub type TokenSpan = Span<Token>;


impl Token {
    pub fn spanned(self, start: usize, end: usize) -> Span<Token> {
        TokenSpan::new(start, end, self)
    }

    pub fn get_identifier(&self) -> Option<String> {
        match self {
            Token::IDENT(id) => Some(id.clone()),
            _ => None,
        }
    }

    pub fn is_ident(&self) -> bool {
        match &self {
            Token::IDENT(_) => true,
            _ => false
        }
    }

    pub fn is_any_literal(&self) -> bool {
        match &self {
            &Token::B_NUMBER(_) | &Token::D_NUMBER(_) | &Token::O_NUMBER(_) | &Token::H_NUMBER(_) | &Token::FD_NUMBER(_) => true,
            &Token::D_LITERAL(_, _) | &Token::S_LITERAL(_, _) => true,
            _ => false
        }
    }

    pub fn spanned_one(self, start: usize) -> Span<Token> {
        TokenSpan::new(start, start, self)
    }
}


impl Default for Token {
    fn default() -> Self {
        Token::L_PAREN
    }
}