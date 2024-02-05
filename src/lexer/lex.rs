use crate::errors::{ErrorReporter, WjlError};
use crate::helpers::treat_as_bigint;
use crate::iter::{wrap_iter, GenericIterator, PeekableIterator};
use crate::tokens::span::Span;
use crate::tokens::Token::{LITERAL_DOUBLE, LITERAL_SINGLE};
use crate::tokens::{IdentKind, Token};
use either::Either;
use std::fmt::Write;

impl PeekableIterator<char> {
    pub fn pull_literal(&mut self, matcher: char, reporter: &mut ErrorReporter) -> String {
        let mut content: String = String::new();
        let mut delim_found = false;
        let start_index = self.get_index().unwrap();
        while let Some(char) = self.next() {
            let is_prev_esc = self.peek_prev().map_or(false, |x| x == '\\');
            if char == matcher && !is_prev_esc {
                delim_found = true;
                break;
            }
            content
                .write_char(char)
                .expect("Failed to write char into buf");
        }
        if !delim_found {
            reporter.add(
                WjlError::char(start_index)
                    .message(format!("Unmatched literal delimiter: {}", matcher))
                    .set_end_char(self.get_content().len() - 1)
                    .ok(),
            );
            return content;
        }
        return content;
    }

    pub fn pull_ident(&mut self) -> String {
        let mut ident_content = String::new();
        ident_content
            .write_char(self.curr().unwrap())
            .expect("Failed to write char into buf");
        while let Some(next) = self.next() {
            if next.is_alphabetic() || next.is_digit(10) || next == '_' {
                ident_content
                    .write_char(next)
                    .expect("Failed to write char into buf");
            } else {
                self.seek(-1);
                break;
            }
        }
        return ident_content;
    }

    // expected that iter is on the first character of the identifier (or backtick)
    pub fn parse_ident(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        let curr = self.curr();
        let start = self.get_index().unwrap();
        if curr.is_none() {
            reporter.add(
                WjlError::char(self.get_index().unwrap())
                    .message("Expected identifier or backtick, got nothing.")
                    .ok(),
            );
            return;
        }
        let curr = curr.unwrap();
        let ident_content = if curr == '`' {
            self.pull_literal('`', reporter)
        } else {
            self.pull_ident()
        };
        let first_ident_end = self.get_index().unwrap();
        let token = self
            .parse_keyword(&ident_content)
            .unwrap_or(Token::IDENT(if curr == '`' {
                IdentKind::BACKTICK(ident_content)
            } else {
                IdentKind::DEFAULT(ident_content)
            }));
        stream.push(token.span(start, first_ident_end));
    }

    fn next_skip_ws(&mut self) -> Option<char> {
        let next = self.next();
        if next.map_or(false, |x| x.is_whitespace()).eq(&true) {
            return self.next_skip_ws();
        }
        next
    }

    pub fn parse_keyword(&self, keyword: &String) -> Option<Token> {
        let keyword = match keyword.as_str() {
            "var" => Token::KEYWORD_VAR,
            "val" => Token::KEYWORD_VAL,
            "const" => Token::KEYWORD_CONST,
            "once" => Token::MOD_KEYWORD_ONCE,
            "public" => Token::MOD_KEYWORD_PUBLIC,
            "protected" => Token::MOD_KEYWORD_PROTECTED,
            "enum" => Token::KEYWORD_ENUM,
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
            _ => Token::NONCE,
        };
        return if keyword != Token::NONCE {
            Some(keyword)
        } else {
            None
        };
    }
    // expecting that we are on a digit
    pub fn parse_decimal_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        let curr = self.curr().unwrap();
        if curr == '0'
            && self
                .peek_next()
                .map_or(false, |x| x.to_ascii_lowercase() == 'o')
                .eq(&true)
        {
            return self.parse_octal_number(stream, reporter);
        }
        if curr == '0'
            && self
                .peek_next()
                .map_or(false, |x| x.to_ascii_lowercase() == 'x')
                .eq(&true)
        {
            return self.parse_hex_number(stream, reporter);
        }

        if curr == '0'
            && self
                .peek_next()
                .map_or(false, |x| x.to_ascii_lowercase() == 'b')
                .eq(&true)
        {
            return self.parse_bin_number(stream, reporter);
        }

        if self
            .peek_next()
            .map_or(false, |x| x.to_ascii_lowercase() == 'e')
            .eq(&true)
        {
            return self.parse_exp_number(stream, reporter);
        }

        let is_negative = self.peek_prev().map_or(false, |x| x == '-');
        let mut buf = if is_negative {
            let mut b = String::from('-');
            b.write_char(curr).expect("Failed to write char to buf");
            b
        } else {
            String::from(curr)
        };
        let mut is_float_from_shorthand = false;
        let start = self.get_index().unwrap();
        let mut end = 0;
        macro_rules! finalize {
            () => {
                {
                    if buf.contains(".") {
                let res = buf.parse::<f64>();
                if res.is_err() {
                    reporter.add(WjlError::char(start)
                        .set_end_char(end)
                        .cause(res.err().unwrap().to_string())
                        .message("Failed to parse float, this is very likely due to the fact that the number is not inside the 64 bit signed float range.")
                        .ok());
                    return
                }
                stream.push(Token::FLOAT(buf).span(start, end));
            } else {
                let res = buf.parse::<i64>();
                if res.is_err() {
                    if treat_as_bigint(buf, 10) {
                        stream.push(Token::INT(res.unwrap()).span(start, end));
                    } else {
                            reporter.add(WjlError::char(start)
                        .set_end_char(end)
                        .cause(res.err().unwrap().to_string())
                        .message("Failed to parse integer, this is very likely due to the fact that the number is not inside the 64 bit signed integer range.").ok());
                    return
                    }
                }
                stream.push(Token::INT(res.unwrap()).span(start, end));
            }
                }
            };
        }
        while let Some(mut char) = self.next() {
            if char.is_digit(10) {
                buf.write_char(char).expect("Failed to write char to buf");
                continue;
            }
            if char == '_' {
                continue; // we allow this but no need to preserve it
            }

            if char == 'f' {
                if buf.contains('.') {
                    reporter.add(
                        WjlError::char(self.get_index().unwrap())
                            .message("Cannot mix decimal notation with <num>f shorthand syntax.")
                            .pot_fix(format!("Remove the f after {}", buf.pop().unwrap())) // safe to pop here as we dont give a single shit about accuracy after an error is reported
                            .ok(),
                    );
                    return;
                }
                buf.write_str(".0").expect("Failed to write char to buf");
                is_float_from_shorthand = true;
                continue;
            }
            if char == '.' {
                let next = self.peek_next();
                if next.is_none() {
                    reporter.add(
                        WjlError::char(self.get_index().unwrap())
                            .message("Expected identifier or number, got nothing.")
                            .ok(),
                    );
                    return;
                }
                let next = next.unwrap();
                if is_float_from_shorthand && next.is_digit(10) {
                    reporter.add(
                        WjlError::char(self.get_index().unwrap())
                            .message("Cannot mix decimal notation with <num>f shorthand syntax.")
                            .pot_fix(format!("Remove the . after {}", buf.pop().unwrap())) // safe to pop here as we dont give a single shit about accuracy after an error is reported
                            .ok(),
                    );
                    return;
                }
                if next.is_valid_ident_start() {
                    let ix = self.get_index().unwrap();
                    end = ix;
                    finalize!();
                    stream.push(Token::PERIOD.span(ix, ix + 1));
                    return;
                }
                buf.write_char(char).expect("Failed to write char to buf");
                continue;
            }
            end = self.get_index().unwrap();
            finalize!();
            self.seek(-1); //give this char back
            return;
        }
    }

    pub fn parse_octal_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        let mut buf = String::new();
        let start = self.get_index().unwrap();
        let is_negative = self.peek_prev().map_or(false, |x| x == '-');
        if is_negative {
            buf.write_char('-').expect("Failed to write char to buf");
        }
        self.next(); //drop the b
        while let Some(char) = self.next() {
            if char.is_digit(8) {
                buf.write_char(char).expect("Failed to write char to buf");
                continue;
            } else if char.is_digit(10) {
                reporter.add(
                    WjlError::char(self.get_index().unwrap())
                        .message(
                            "Invalid octal number. Only numbers 0-7 are allowed in octal numbers.",
                        )
                        .ok(),
                );
                return;
            }
            self.seek(-1);
            break;
        }
        // buf.parse fails here since it assumes a 10 based radix
        let i64_res = i64::from_str_radix(&*buf, 8);
        if i64_res.is_err() {
            reporter.add(WjlError::char(start)
                .set_end_char(self.get_index().unwrap())
                .cause(i64_res.err().unwrap().to_string())
                .message("Failed to parse octal integer, this is very likely due to the fact that the number is not inside the 64 bit signed integer range.").ok());
            return;
        }
        stream.push(Token::OCTAL_NUMBER(buf).span(start, self.get_index().unwrap()));
    }

    pub fn parse_hex_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        let mut buf = String::new();
        let start = self.get_index().unwrap();
        let is_negative = self.peek_prev().map_or(false, |x| x == '-');
        if is_negative {
            buf.write_char('-').expect("Failed to write char to buf");
        }
        self.next(); //drop the b
        while let Some(char) = self.next() {
            if char.is_digit(16) {
                buf.write_char(char).expect("Failed to write char to buf");
                continue;
            }
            self.seek(-1);
            break;
        }
        // buf.parse fails here since it assumes a 10 based radix
        let i64_res = i64::from_str_radix(&*buf, 16);
        if i64_res.is_err() {
            reporter.add(WjlError::char(start)
                .set_end_char(self.get_index().unwrap())
                .cause(i64_res.err().unwrap().to_string())
                .message("Failed to parse hex integer, this is very likely due to the fact that the number is not inside the 64 bit signed integer range.").ok());
            return;
        }
        stream.push(Token::HEX_NUMBER(buf).span(start, self.get_index().unwrap()));
    }

    pub fn parse_exp_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {}

    pub fn parse_bin_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        let mut buf = String::new();
        let start = self.get_index().unwrap();
        let is_negative = self.peek_prev().map_or(false, |x| x == '-');
        if is_negative {
            buf.write_char('-').expect("Failed to write char to buf");
        }
        self.next(); //drop the b
        while let Some(char) = self.next() {
            if char.is_digit(2) {
                buf.write_char(char).expect("Failed to write char to buf");
                continue;
            } else if char.is_digit(10) {
                reporter.add(
                    WjlError::char(self.get_index().unwrap())
                        .message(
                            "Invalid binary number. Only 0s and 1s are allowed in binary numbers.",
                        )
                        .ok(),
                );
                return;
            }
            self.seek(-1);
            break;
        }
        // buf.parse fails here since it assumes a 10 based radix
        let i64_res = i64::from_str_radix(&*buf, 2);
        if i64_res.is_err() {
            reporter.add(WjlError::char(start)
                .set_end_char(self.get_index().unwrap())
                .cause(i64_res.err().unwrap().to_string())
                .message("Failed to parse binary integer, this is very likely due to the fact that the number is not inside the 64 bit signed integer range.").ok());
            return;
        }
        stream.push(Token::BINARY_NUMBER(buf).span(start, self.get_index().unwrap()));
    }
}

pub fn lex_stream(input: &String, reporter: &mut ErrorReporter) -> Vec<Span> {
    let chars = input.chars().collect::<Vec<char>>();
    let mut iter = wrap_iter(chars);
    let mut stream: Vec<Span> = vec![];
    while let Some(char) = iter.next() {
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
        }
        if char == '"' {
            let curr = iter.get_index().unwrap();
            let lit = iter.pull_literal('"', reporter);
            let lit_end = iter.get_index().unwrap();
            stream.push(LITERAL_DOUBLE(vec![Either::Left(lit)]).span(curr, lit_end)); //TODO! parse literals
            continue;
        }
        if char == '\'' {
            let curr = iter.get_index().unwrap();
            let lit = iter.pull_literal('"', reporter);
            let lit_end = iter.get_index().unwrap();
            stream.push(LITERAL_SINGLE(vec![Either::Left(lit)]).span(curr, lit_end));
            continue;
        }

        if char.is_digit(10) {
            iter.parse_decimal_number(&mut stream, reporter);
            continue;
        }

        // check binary, hex and octal numbers here

        if char.is_valid_ident_start() {
            iter.parse_ident(&mut stream, reporter);
            continue;
        }

        let tok = match char {
            '!' => Token::EXCL_MARK,
            '#' => Token::HASH,
            '.' => Token::PERIOD,
            '<' => Token::ANGLE_LEFT,
            '>' => Token::ANGLE_RIGHT,
            '(' => Token::PAREN_LEFT,
            ')' => Token::PAREN_RIGHT,
            '[' => Token::BRACKET_LEFT,
            ']' => Token::BRACKET_RIGHT,
            '{' => Token::BRACE_LEFT,
            '}' => Token::BRACE_RIGHT,
            '?' => Token::QMARK,
            '-' => Token::SUBTRACT,
            '+' => Token::SUM,
            '*' => Token::MUL,
            '/' => Token::DIV,
            '%' => Token::MOD,
            '=' => Token::ASSIGN,
            '|' => Token::PIPE,
            '@' => Token::AT,
            ';' => Token::DELIMITER,
            _ if char.is_whitespace() => Token::WHITESPACE,
            _ if char == '\n' => Token::LINE_BREAK,
            _ => Token::NONCE,
        };
    }
    stream
}

trait IsValidIdentStart {
    fn is_valid_ident_start(&self) -> bool;
}

impl IsValidIdentStart for char {
    fn is_valid_ident_start(&self) -> bool {
        self.is_alphabetic() || self == &'_' || self == &'`'
    }
}
