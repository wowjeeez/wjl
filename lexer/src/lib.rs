use std::fmt::Write;
use common::iter::SmartIterator;
use common::span::Span;
use common::token::Token;

pub fn lex_string(input: String) -> Vec<Span<Token>> {
    let chars =  input.chars().collect::<Vec<char>>();
    let mut token_stream = vec![];
    let mut stream = SmartIterator::new(chars);
    let mut current_lit_proto:  Option<fn(String) -> Token> = None;
    let mut current_lit_start: Option<usize> = None;
    let mut current_lit_content: String = String::new();

    while let Some((current, current_index)) = stream.next() {
        let current = current.clone();
        let is_in_literal = current_lit_proto.is_some();
        let mut save_ident = |lit_content: &mut String| {
            let start = current_lit_start.take();
            let proto = current_lit_proto.take();

            if let (Some(start), Some(token_ctor)) = (start, proto) {
                let end = stream.get_previous_index();
                let mut token = token_ctor(lit_content.clone());
                if let Token::IDENT(ref ident) = token {
                    token = match ident.as_str() {
                        _ => token
                    }
                }
                token_stream.push(token.spanned(start, end));
                lit_content.clear();
            }
        };
        if current == '"' || current == '\'' {
            let is_double = current == '"';
            let mut literal_content = String::new();
            let mut needs_parsing = false;
            'lit: while let  Some((character, _)) = stream.next() {
                let character = character.clone();
                if (character == '"' && is_double) || character == '\'' {
                    break 'lit
                }
                needs_parsing = needs_parsing || character == '$' && !stream.lookbehind_is(&'\\');
                literal_content.write_char(character).unwrap();
            }
            let token = if is_double { Token::D_LITERAL(literal_content, needs_parsing) } else {Token::S_LITERAL(literal_content, needs_parsing)};
            token_stream.push(token.spanned(current_index, stream.get_current_index()));
            continue
        }


        let op = match current {
            '(' => Token::L_PAREN,
            ')' => Token::R_PAREN,
            '{' => Token::L_CURLY,
            '}' => Token::R_CURLY,
            '[' => Token::L_BRACKET,
            ']' => Token::R_BRACKET,
            '<' => Token::L_ANGLE,
            '>' => Token::R_ANGLE,
            '=' => Token::EQ,
            '!' => Token::EXCL,
            '?' => Token::QMARK,
            '+' => Token::SUM,
            '-' => Token::SUB,
            '/' => Token::DIV,
            '*' => Token::MUL,
            '%' => Token::MOD,
            ':' => Token::COLON,
            '|' => Token::PIPE,
            '&' => Token::AMP,
            ';' => Token::SEMICOLON,
            '.' => Token::PERIOD,
            '#' => Token::HASH,
            _ => Token::WHITESPACE_MERGED,
        };
        if op != Token::WHITESPACE_MERGED {
            save_ident(&mut current_lit_content);
            token_stream.push(op.spanned_one(stream.get_current_index()));
            continue
        }

        if current == '0' && ['b', 'o', 'x'].contains(stream.peek().unwrap_or(&'_')) {
            save_ident(&mut current_lit_content);
            let (num_type, _) = stream.next().unwrap();
            if num_type == &'b' {
                current_lit_proto = Some(Token::B_NUMBER);
            } else if num_type == &'o' {
                current_lit_proto = Some(Token::O_NUMBER);
            }  else if num_type == &'x' {
                current_lit_proto = Some(Token::H_NUMBER);
            }
            current_lit_start = Some(current_index);
            continue
        }

        if current.is_digit(10) && !is_in_literal {
            current_lit_proto = Some(Token::D_NUMBER);
            current_lit_start = Some(current_index);
            current_lit_content.write_char(current).unwrap();
            continue
        }

        if stream.lookbehind_is_fn(|t| t.is_digit(10)) && current == '_' {
            continue
        }
        if current.is_whitespace() {
            save_ident(&mut current_lit_content);
            'whitespace: loop {
                if stream.peek_is_fn(|t| t.is_whitespace()) {
                    stream.next();
                } else {
                    break 'whitespace
                }
            }
            token_stream.push(Token::WHITESPACE_MERGED.spanned(current_index, stream.get_current_index()));
            continue
        }

        if current.is_alphabetic() && !is_in_literal {
            current_lit_proto = Some(Token::IDENT);
            current_lit_start = Some(current_index);
            current_lit_content.write_char(current).unwrap();
            continue
        }
        current_lit_content.write_char(current).unwrap();

        if stream.peek().is_none() {
            save_ident(&mut current_lit_content);
        }
    }

    token_stream
}