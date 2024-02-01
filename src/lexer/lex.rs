use std::fmt::Write;
use either::Either;
use crate::errors::{ErrorReporter, WjlError};
use crate::iter::{GenericIterator, PeekableIterator, wrap_iter};
use crate::tokens::{PathSegment, Token};
use crate::tokens::span::Span;
use crate::tokens::Token::{LITERAL_DOUBLE, LITERAL_SINGLE, PATH};

impl PeekableIterator<char> {
    pub fn pull_literal(&mut self, matcher: char, reporter: &mut ErrorReporter) -> String {
        let mut content: String = String::new();
        let mut delim_found = false;
        let start_index = self.get_index().unwrap();
        while let Some(char) = self.next() {
            let is_prev_esc = self.peek_prev().map_or(false, |x| x == '\\');
            if char == matcher && !is_prev_esc {
                delim_found = true;
                break
            }
            content.write_char(char).expect("Failed to write char into buf");
        }
        if !delim_found {
            reporter.add(WjlError::char(start_index)
                .message(format!("Unmatched literal delimiter: {}", matcher))
                .end_char(self.get_content().len() - 1).ok()
            );
            return content
        }
        return content
    }

    pub fn pull_ident(&mut self) -> String {
        let mut ident_content = String::new();
        ident_content.write_char(self.curr().unwrap()).expect("Failed to write char into buf");
        while let Some(next) = self.next() {
            if next.is_alphabetic() || next.is_digit(10) || next == '_' {
                ident_content.write_char(next).expect("Failed to write char into buf");
            } else {
                self.seek(-1);
                break
            }
        }
        return ident_content
    }
}

pub fn lex_stream(input: &String, reporter: &mut ErrorReporter) -> Vec<Span> {
    let chars = input.chars().collect::<Vec<char>>();
    let mut iter = wrap_iter(chars);
    let mut stream: Vec<Span> = vec![];
    'don: while let Some(char) = iter.next() {
        let last_token = stream.last();
        if last_token.is_some() {
            let last_token = last_token.unwrap().clone();
            if last_token.get_token() == Token::KEYWORD_IF {
                let token = stream.get(stream.len() - 2).unwrap();
                if token.get_token() == Token::KEYWORD_ELSE {
                    let start = token.start;
                    let end = last_token.end;
                    stream.pop();
                    stream.pop();
                    stream.push(Token::KEYWORD_ELSE_IF.span(start, end));
                }
            }
            if let Some(ident_name) = last_token.get_token().get_if_is_1_len_ident_no_bt() {
                let keyword = match ident_name.as_str() {
                    "var" => Token::KEYWORD_VAR,
                    "val" => Token::KEYWORD_VAL,
                    "const" => Token::KEYWORD_CONST,
                    "once" => Token::MOD_KEYWORD_ONCE,
                    "public" => Token::MOD_KEYWORD_PUBLIC,
                    "protected" => Token::MOD_KEYWORD_PROTECTED,
                    "internal" => Token::MOD_KEYWORD_INTERNAL,
                    "func" => Token::KEYWORD_FUNC,
                    "class" => Token::KEYWORD_CLASS,
                    "impl" => Token::KEYWORD_IMPL,
                    "for" => Token::KEYWORD_FOR,
                    "return" => Token::KEYWORD_RETURN,
                    "break" => Token::KEYWORD_BREAK,
                    "continue" => Token::KEYWORD_CONTINUE,
                    "struct" => Token::KEYWORD_STRUCT,
                    "await" => Token::KEYWORD_AWAIT,
                    "in" => Token::KEYWORD_IN,
                    "while" => Token::KEYWORD_WHILE,
                    "match" => Token::KEYWORD_MATCH,
                    "try" => Token::KEYWORD_TRY,
                    "catch" => Token::KEYWORD_CATCH,
                    "this" => Token::KEYWORD_THIS,
                    "type" => Token::KEYWORD_TYPE,
                    "constructor" => Token::KEYWORD_CONSTRUCTOR,
                    "classdef" => Token::KEYWORD_CLASSDEF,
                    "funcdef" => Token::KEYWORD_FUNCDEF,
                    "use" => Token::KEYWORD_USE,
                    "ext" => Token::KEYWORD_EXT,
                    "operator" => Token::KEYWORD_OPERATOR,
                    "decorator" => Token::KEYWORD_DECORATOR,
                    "reflect" => Token::KEYWORD_REFLECT,
                    "interface" => Token::KEYWORD_INTERFACE,
                    "if" => Token::KEYWORD_IF,
                    "else" => Token::KEYWORD_ELSE,
                    "yield" => Token::KEYWORD_YIELD,
                    _ => Token::NONCE
                };

                if keyword != Token::NONCE {
                    let prev = stream.pop().unwrap();
                    stream.push(keyword.span(prev.start, prev.end));
                }
            }
        }
        if char == '"' {
            let curr = iter.get_index().unwrap();
            let lit = iter.pull_literal('"', reporter);
            let lit_end = iter.get_index().unwrap();
            stream.push(LITERAL_DOUBLE(vec![Either::Left(lit)]).span(curr, lit_end)); //TODO! parse literals
            continue
        }
        if char == '\'' {
            let curr = iter.get_index().unwrap();
            let lit = iter.pull_literal('"', reporter);
            let lit_end = iter.get_index().unwrap();
            stream.push(LITERAL_SINGLE(vec![Either::Left(lit)]).span(curr, lit_end));
            continue
        }

        if char.is_alphabetic() || char == '_' || char == '`' {
            let start = iter.get_index().unwrap();
            let ident = if char == '`' {iter.pull_literal('`', reporter)} else {iter.pull_ident()};
            let ident = if char == '`' {
                PathSegment::backtick(ident)
            } else {
                PathSegment::default(ident)
            };
            let end = iter.get_index().unwrap();
            let curr = iter.curr().unwrap();
            let is_dbc_sep = curr == ':' && iter.peek_next().map_or(false, |x| x == ':');
            if curr != '.' && !is_dbc_sep  {
                stream.push(PATH(vec![ident]).span(start, end));
                continue
            }
            let mut inner_stream = vec![ident];
            if is_dbc_sep {
                iter.next(); //drop the second :
            }
            let next = iter.next();
            if next.is_none() {
                let curr = iter.get_index().unwrap() - 1;
                reporter.add(WjlError::char(curr).message("Expected identifier, got nothing.").ok());
                break
            }
            let next = next.unwrap();
            if !next.is_alphabetic() && next != '_' && next != '`' {
                let curr = iter.get_index().unwrap();
                reporter.add(WjlError::char(curr).message(format!("Expected identifier, got: {}.", char)).ok());
                break
            }
            let ident = if next == '`' {iter.pull_literal('`', reporter)} else {iter.pull_ident()};
            let ident = if next == '`' {
                PathSegment::backtick_colon(ident, is_dbc_sep)
            } else {
                PathSegment::default_colon(ident, is_dbc_sep)
            };
            inner_stream.push(ident);

            let next = iter.peek_next();
            if next.is_none() {
                let curr = iter.get_index().unwrap() - 1;
                reporter.add(WjlError::char(curr).message("Expected qualified path or expression, got nothing.").ok());
                break
            }
            let next = next.unwrap();
            if next != ':' && next != '.' {
                stream.push(PATH(inner_stream).span(start, iter.get_index().unwrap()));
                continue
            }

            // loop cycle expects to be on a path on its start (. or :)
            'inner: while let next = iter.next() {
                if next.is_none() {
                    let ix = iter.get_index().unwrap() - 1;
                    let prev = iter.peek_prev().unwrap();
                    if prev == '.' || prev == ':' {
                        reporter.add(WjlError::char(ix).message("Expected identifier, got nothing.").ok());
                    } else {
                        reporter.add(WjlError::char(ix).message("Unfinished statement/expression, got nothing.").ok());
                    }
                    break 'don
                }

                let next = next.unwrap();
                if next == '.' || (next == ':' && iter.peek_next().map_or(false, |x| x == ':')) {
                    let is_colon = next != '.'; // easier and faster instead of peeking again
                    let next = iter.next();
                    if next.is_none() {
                        let ix = iter.get_index().unwrap() - 1;
                        reporter.add(WjlError::char(ix).message("Expected identifier, got nothing.").ok());
                        break 'don
                    }
                    let next = next.unwrap();
                    if next.is_alphabetic() || next == '_' || next == '`' {
                        let ident = if next == '`' {iter.pull_literal('`', reporter)} else {iter.pull_ident()};
                        inner_stream.push(if next == '`' {PathSegment::backtick_colon(ident, is_colon)} else {PathSegment::default_colon(ident, is_colon)});
                    } else {
                        let ix = iter.get_index().unwrap();
                        reporter.add(WjlError::char(ix).message(format!("Expected identifier, got: {}.", next)).ok());
                        break 'don
                    }

                } else {
                    stream.push(PATH(inner_stream).span(start, iter.get_index().unwrap()));
                    break 'inner
                }
            }
        }

    }
    stream
}