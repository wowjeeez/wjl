use crate::errors::{ErrorReporter, WjlError};
use crate::helpers::{Print, treat_as_bigint};
use crate::iter::{wrap_iter, GenericIterator, PeekableIterator};
use crate::tokens::span::Span;
use crate::tokens::Token::{LITERAL_DOUBLE, LITERAL_SINGLE};
use crate::tokens::{IdentKind, Token};
use either::Either;
use std::fmt::Write;
use colored::Colorize;

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
                    if treat_as_bigint(&buf, 10) {
                        stream.push(Token::INT(buf, true).span(start, end));
                    } else {
                        reporter.add(WjlError::char(start)
                        .set_end_char(end)
                        .cause(res.err().unwrap().to_string())
                        .message("Failed to parse integer.").ok());
                    }
                    return
                }
                stream.push(Token::INT(buf, false).span(start, end));
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
            if treat_as_bigint(&buf, 8) {
                stream.push(Token::OCTAL_NUMBER(buf, true).span(start, self.get_index().unwrap()));
            } else {
                reporter.add(WjlError::char(start)
                    .set_end_char(self.get_index().unwrap())
                    .cause(i64_res.err().unwrap().to_string())
                    .message("Failed to parse octal integer.").ok());
            }
            return;
        }
        stream.push(Token::OCTAL_NUMBER(buf, false).span(start, self.get_index().unwrap()));
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
            if treat_as_bigint(&buf, 16) {
                stream.push(Token::HEX_NUMBER(buf, true).span(start, self.get_index().unwrap()));
            } else {
                reporter.add(WjlError::char(start)
                    .set_end_char(self.get_index().unwrap())
                    .cause(i64_res.err().unwrap().to_string())
                    .message("Failed to parse hex integer.").ok());
            }
            return;
        }
        stream.push(Token::HEX_NUMBER(buf, false).span(start, self.get_index().unwrap()));
    }

    pub fn parse_exp_number(&mut self, stream: &mut Vec<Span>, reporter: &mut ErrorReporter) {
        todo!()
    }

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
            if treat_as_bigint(&buf, 2) {
                stream.push(Token::BINARY_NUMBER(buf, true).span(start, self.get_index().unwrap()));
            } else {
                reporter.add(WjlError::char(start)
                    .set_end_char(self.get_index().unwrap())
                    .cause(i64_res.err().unwrap().to_string())
                    .message("Failed to parse binary integer.").ok());
            }
            return;
        }
        stream.push(Token::BINARY_NUMBER(buf, false).span(start, self.get_index().unwrap()));
    }
}

fn match_char(char: char) -> Token {
    match char {
        '!' => Token::EXCL_MARK,
        '#' => Token::HASH,
        '.' => Token::PERIOD,
        '<' => Token::ANGLE_LEFT,
        '>' => Token::ANGLE_RIGHT,
        ':' => Token::COLON,
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
        '&' => Token::BIT_AND,
        '~' => Token::BIT_NOT,
        '^' => Token::BIT_XOR,
        _ if char == '\n' => Token::LINE_BREAK,
        _ if char.is_whitespace() => Token::WHITESPACE,
        _ => Token::NONCE,
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

        if char.is_valid_ident_start() {
            iter.parse_ident(&mut stream, reporter);
            continue;
        }

        let tok = match_char(char);
        let next_tok = iter.peek_next();
        let start = iter.get_index().unwrap();
        if next_tok.is_none() {
            stream.push(tok.span(start, start));
            continue
        }
        let next_tok = next_tok.unwrap();
        let next_tok = match_char(next_tok);
        let mut push_t = |tok: Token| {
            iter.next();
            stream.push(tok.span(start, start + 1))
        };

        if tok == Token::PIPE && next_tok == Token::PIPE {
            push_t(Token::OR);
            continue
        }
        if tok == Token::PIPE && next_tok == Token::ANGLE_RIGHT {
            push_t(Token::PIPE_OP);
            continue
        }
        if tok == Token::ANGLE_LEFT && next_tok == Token::ASSIGN {
            push_t(Token::RIGHT_GTE);
            continue
        }
        if tok == Token::ANGLE_RIGHT && next_tok == Token::ASSIGN {
            push_t(Token::LEFT_GTE);
            continue
        }
        if tok == Token::ASSIGN && next_tok == Token::ASSIGN {
            push_t(Token::EQ);
            continue
        }
        if tok == Token::EXCL_MARK && next_tok == Token::ASSIGN {
            push_t(Token::N_EQ);
            continue
        }
        if tok == Token::MUL && next_tok == Token::ASSIGN {
            push_t(Token::ASSIGN_MUL);
            continue
        }
        if tok == Token::SUM && next_tok == Token::ASSIGN {
            push_t(Token::ASSIGN_SUM);
            continue
        }
        if tok == Token::DIV && next_tok == Token::ASSIGN {
            push_t(Token::ASSIGN_DIV);
            continue
        }
        if tok == Token::MOD && next_tok == Token::ASSIGN {
            push_t(Token::ASSIGN_MOD);
            continue
        }
        if tok == Token::SUBTRACT && next_tok == Token::ASSIGN {
            push_t(Token::ASSIGN_SUB);
            continue
        }
        if tok == Token::COLON && next_tok == Token::COLON {
            push_t(Token::DOUBLE_COLON);
            continue
        }
        if tok == Token::BIT_AND && next_tok == Token::BIT_AND {
            push_t(Token::AND);
            continue
        }
        if tok == Token::ANGLE_LEFT && next_tok == Token::ANGLE_LEFT {
            push_t(Token::BIT_ZERO_FILL_LEFT_SHIFT);
            continue
        }
        if tok == Token::ANGLE_RIGHT && next_tok == Token::ANGLE_RIGHT {
            push_t(Token::BIT_S_RIGHT_SHIFT);
            continue
        }
        if tok == Token::ANGLE_LEFT && next_tok == Token::ANGLE_LEFT && iter.peek_n(2).map_or(false, |x| x == '>') {
            stream.push(Token::BIT_ZERO_FILL_RIGHT_SHIFT.span(start, start + 2));
            continue
        }
        if tok != Token::NONCE {
            stream.push(tok.span(start, start));
            continue
        }
        reporter.add(WjlError::char(start).message(format!("Unexpected character: {}", char)).ok());
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

impl Print for Vec<Span> {
    fn print(&self) -> String {
        let mut ret = String::new();
        for tok in self {
            let token = tok.get_token();
            let str = match token {
                Token::ANGLE_LEFT => "<".bright_yellow(),
                Token::ANGLE_RIGHT => ">".bright_yellow(),
                Token::BRACE_LEFT => "{".bright_yellow(),
                Token::BRACE_RIGHT => "}".bright_yellow(),
                Token::PAREN_LEFT => "(".bright_yellow(),
                Token::PAREN_RIGHT => ")".bright_yellow(),
                Token::BRACKET_LEFT => "[".bright_yellow(),
                Token::BRACKET_RIGHT => "]".bright_yellow(),
                Token::LEFT_GTE => ">=".green(),
                Token::RIGHT_GTE => "<=".green(),
                Token::EQ => "==".green(),
                Token::N_EQ => "!=".green(),
                Token::COLON => ":".green(),
                Token::SUBTRACT => "-".green(),
                Token::SUM => "+".green(),
                Token::MUL => "*".green(),
                Token::DIV => "/".green(),
                Token::MOD => "%".green(),
                Token::BIT_AND => "&".green(),
                Token::AND => "&&".green(),
                Token::OR => "||".green(),
                Token::BIT_XOR => "^".green(),
                Token::BIT_NOT => "~".green(),
                Token::BIT_ZERO_FILL_LEFT_SHIFT => "<<".green(),
                Token::BIT_S_RIGHT_SHIFT => ">>".green(),
                Token::BIT_ZERO_FILL_RIGHT_SHIFT => ">>>".green(),
                Token::HASH => "#".green(),
                Token::AT => "@".green(),
                Token::QMARK => "?".green(),
                Token::EXCL_MARK => "!".green(),
                Token::BACKSLASH => "\\".green(),
                Token::COMMA => ",".green(),
                Token::SEMI_COLON => ";".red(),
                Token::LINE_BREAK => "\n".into(),
                Token::PIPE => "|".green(),
                Token::PERIOD => ".".green(),
                Token::DOUBLE_COLON => "::".green(),
                Token::KEYWORD_VAR => "var".blue(),
                Token::KEYWORD_VAL => "val".blue(),
                Token::KEYWORD_CONST => "const".blue(),
                Token::MOD_KEYWORD_ONCE => "once".blue(),
                Token::MOD_KEYWORD_PUBLIC => "public".blue(),
                Token::MOD_KEYWORD_PROTECTED => "protected".blue(),
                Token::MOD_KEYWORD_INTERNAL => "internal".blue(),
                Token::KEYWORD_FUNC => "func".blue(),
                Token::KEYWORD_CLASS => "class".blue(),
                Token::KEYWORD_IMPL => "impl".blue(),
                Token::KEYWORD_FOR => "for".blue(),
                Token::KEYWORD_RETURN => "return".blue(),
                Token::KEYWORD_BREAK => "break".blue(),
                Token::KEYWORD_CONTINUE => "continue".blue(),
                Token::KEYWORD_STRUCT => "struct".blue(),
                Token::KEYWORD_AWAIT => "await".blue(),
                Token::KEYWORD_IN => "in".blue(),
                Token::KEYWORD_WHILE => "while".blue(),
                Token::KEYWORD_MATCH => "match".blue(),
                Token::KEYWORD_ENUM => "enum".blue(),
                Token::KEYWORD_TRY => "try".blue(),
                Token::KEYWORD_CATCH => "catch".blue(),
                Token::KEYWORD_THIS => "this".red(),
                Token::KEYWORD_TYPE => "type".blue(),
                Token::KEYWORD_CONSTRUCTOR => "constructor".red(),
                Token::KEYWORD_CLASSDEF => "clasdef".blue(),
                Token::KEYWORD_FUNCDEF => "funcdef".blue(),
                Token::KEYWORD_USE => "use".blue(),
                Token::KEYWORD_EXT => "external".blue(),
                Token::KEYWORD_OPERATOR => "operator".blue(),
                Token::KEYWORD_DECORATOR => "decorator".blue(),
                Token::KEYWORD_REFLECT => "reflect".blue(),
                Token::KEYWORD_INTERFACE => "interface".blue(),
                Token::KEYWORD_IF => "if".blue(),
                Token::KEYWORD_ELSE => "else".blue(),
                Token::KEYWORD_ELSE_IF => "else if".blue(),
                Token::KEYWORD_YIELD => "yield".blue(),
                Token::OP_SPREAD => "...".red(),
                Token::IDENT(kind) => match kind {
                    IdentKind::DEFAULT(content) => format!("[IDENT_DEFAULT: {}]", content).red(),
                    IdentKind::BACKTICK(content) => format!("[IDENT_BACKTICK: {}]", content).red()
                },
                Token::ASSIGN => "=".green(),
                Token::ASSIGN_SUM => "+=".green(),
                Token::ASSIGN_DIV => "/=".green(),
                Token::ASSIGN_MOD => "%=".green(),
                Token::ASSIGN_MUL => "*=".green(),
                Token::ASSIGN_SUB => "-=".green(),
                Token::INCR => "++".green(),
                Token::DECR => "--".green(),
                Token::PWR => "**".green(),
                Token::ARROW => "=>".green(),
                LITERAL_DOUBLE(_) => "<double quote literal>".red(),
                LITERAL_SINGLE(_) => "<single quote literal>".red(),
                Token::OP_FUNC_SUM => "sum".green(),
                Token::OP_FUNC_DIV => "div".green(),
                Token::OP_FUNC_MOD => "mod".green(),
                Token::OP_FUNC_SUB => "sub".green(),
                Token::WJL_COMPILER_PLACEHOLDER => "@@wjl_internal".black(),
                Token::DELIMITER => ";".green(),
                Token::WHITESPACE => " ".into(),
                Token::PIPE_OP => "|>".yellow(),
                Token::FLOAT(f) => format!("[float: {}]", f).red(),
                Token::INT(int, bigint) => format!("[int: {}, is_bigint: {}]", int, bigint).red(),
                Token::BINARY_NUMBER(int, bigint) => format!("[binary: {}, is_bigint: {}]", int, bigint).red(),
                Token::OCTAL_NUMBER(int, bigint) => format!("[octal: {}, is_bigint: {}]", int, bigint).red(),
                Token::HEX_NUMBER(int, bigint) => format!("[hex: {}, is_bigint: {}]", int, bigint).red(),
                Token::NONCE => "<<NONCE>>".black()
            };
            ret.write_str(&*str).expect("Failed to write char to stream");
        }
        ret
    }
}