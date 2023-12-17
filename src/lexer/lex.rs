// Stage 1: filters out identifiers, keywords, string literals and individual number characters
// Stage 2: combines import paths and numeric literals into single tokens, fixes up identifiers split into 2 due to using underline in their name and handles equality operations, elvis etc (e.g += -> TOKEN::INCR(

use std::fmt::Write;
use std::vec;
use crate::iter_util::CommonIter;
use crate::lexer::error::LexError;
use crate::lexer::iter::{CharIterator, TokenIter};
use crate::lexer::span::{fill_line_nums, TokenSpan};
use crate::lexer::tokens::TOKEN;

pub fn tokenize(input: String) -> Result<Vec<TokenSpan>, LexError> {
    let len = input.len();
    let mut iter = CharIterator::wrap_string(input.clone());
    let mut token_stream = vec![];
    let mut current_ident = String::new();
    let mut iter_count: isize = -1;
    while let Some(char) = iter.next() {
        iter_count = iter.current_index() as isize;
        let mut save_ident = || {
            let ident = current_ident.trim().to_string();
            if ident.len() != 0 {
                token_stream.push(TOKEN::IDENTIFIER(ident).into_span((iter_count - current_ident.len() as isize) as usize, (iter_count - 1) as usize));
            }
            current_ident.clear();
        };


        let next = iter.peek_next();
        if char == '"' {
            let start = iter_count;
            let literal = iter.eat_until_next_ignore_escaped('"');
            save_ident();
            token_stream.push(TOKEN::STRING_LITERAL(literal).into_span(start as usize, iter.current_index()));
            continue
        }
        if char == '`' {
            let start = iter_count;
            let literal = iter.eat_until_next_ignore_escaped('`');
            save_ident();
            token_stream.push(TOKEN::BACKTICK_LITERAL(literal).into_span(start as usize, iter.current_index()));
            continue
        }
        if char == '/' && next.map_or(false, |x| x == '/') == true {
            let start = iter_count;
            iter.next(); //drop next /
            let comment = iter.eat_until_newline();
            save_ident();
            token_stream.push(TOKEN::COMMENT(comment).into_span(start as usize, iter.current_index()));
            continue
        }
        if char == '/' && next.map_or(false, |x| x == '*') == true {
            let start = iter_count;
            iter.next(); //drop the *
            let eaten = iter.eat_until(|curr, next| curr == '*' && next.map_or(true, |x| x == '/') == true);
            iter.next(); //drop the trailing /
            let split = eaten.split("\n").collect::<Vec<&str>>();
            save_ident();
            let collected = split.into_iter().map(|x|x.trim().to_string()).filter(|x| x.len() != 0).collect();
            token_stream.push(TOKEN::COMMENT_MLINE(collected).into_span(start as usize, iter.current_index()));
            continue
        }

        if char == '\'' {
            let start = iter_count;
            let literal = iter.eat_until_next_ignore_escaped('\'');
            let token = TOKEN::STRING_LITERAL(literal);
            save_ident();
            token_stream.push(token.into_span(start as usize, iter.current_index()));
            continue
        }
        if char.is_ascii_digit() {
            save_ident();
            token_stream.push(TOKEN::NUMBER(char.to_digit(10).unwrap() as u8).into_span(iter.current_index(), iter.current_index()));
            continue
        }
        let prev = iter.peek_prev();
        let (lit, index) = iter.peek_until_whitespace_or_angle();
        let matched_lit = if prev.is_none() || {
            let prev = prev.unwrap();
            prev.is_whitespace() || (!prev.is_alphabetic() && char != '_')
        } {match lit.as_str() {
            "if" => TOKEN::KEYWORD_IF,
            "else" => TOKEN::KEYWORD_ELSE,
            "enum" => TOKEN::KEYWORD_ENUM,
            "class" => TOKEN::KEYWORD_CLASS,
            "struct" => TOKEN::KEYWORD_STRUCT,
            "static" => TOKEN::KEYWORD_STATIC,
            "try" => TOKEN::KEYWORD_TRY,
            "catch" => TOKEN::KEYWORD_CATCH,
            "unwind" => TOKEN::KEYWORD_UNWIND,
            "unsafe" => TOKEN::KEYWORD_UNSAFE,
            "dep" => TOKEN::DIRECTIVE_DEP,
            "edep" => TOKEN::DIRECTIVE_EXTERNAL_DEP,
            "external" => TOKEN::ACCESSMOD_EXTERNAL,
            "internal" => TOKEN::ACCESSMOD_INTERNAL,
            "private" => TOKEN::ACCESSMOD_PRIVATE,
            "val" => TOKEN::DECL_VAL,
            "var" => TOKEN::DECL_VAR,
            "const" => TOKEN::DECL_CONST,
            "once" => TOKEN::DECL_ONCE,
            "match" => TOKEN::KEYWORD_MATCH,
            "typelayer" => TOKEN::KEYWORD_TYPELAYER,
            "interface" => TOKEN::KEYWORD_INTERFACE,
            "async" => TOKEN::KEYWORD_ASYNC,
            "raise" => TOKEN::KEYWORD_RAISE,
            "abstract" => TOKEN::KEYWORD_ABSTRACT,
            "final" => TOKEN::KEYWORD_FINAL,
            "func" => TOKEN::KEYWORD_FUNC,
            "return" => TOKEN::KEYWORD_RETURN,
            "break" => TOKEN::KEYWORD_BREAK,
            "continue" => TOKEN::KEYWORD_CONTINUE,
            "yield" => TOKEN::KEYWORD_YIELD,
            "for" => TOKEN::KEYWORD_FOR,
            "while" => TOKEN::KEYWORD_WHILE,
            "operator" => TOKEN::KEYWORD_OPERATOR,
            "then" => TOKEN::KEYWORD_TYPE_THEN,
            "end" => TOKEN::KEYWORD_TYPE_END,
            "in" => TOKEN::KEYWORD_IN,
            "is" => TOKEN::KEYWORD_IS,
            "exact" => TOKEN::KEYWORD_EXACT,
            "typedef" => TOKEN::KEYWORD_TYPEDEF,
            "true" => TOKEN::TRUE,
            "false" => TOKEN::FALSE,
            "pure" => TOKEN::KEYWORD_PURE,
            "decorator" => TOKEN::KEYWORD_DECORATOR,
            "null" => TOKEN::KEYWORD_NULL,
            _ => TOKEN::_NONCE
        }} else {TOKEN::_NONCE};
        if matched_lit != TOKEN::_NONCE {
                save_ident();
                let start = iter_count;
                iter.seek(index -1);
                token_stream.push(matched_lit.into_span(start as usize, iter.current_index()));
                continue
        }


        let matched_single = match char {
            '{' => TOKEN::LEFT_BRACE,
            '}' => TOKEN::RIGHT_BRACE,
            '(' => TOKEN::LEFT_PAREN,
            ')' => TOKEN::RIGHT_PAREN,
            '[' => TOKEN::LEFT_BRACKET,
            ']' => TOKEN::RIGHT_BRACKET,
            '<' => TOKEN::LEFT_ANGLE,
            '>' => TOKEN::RIGHT_ANGLE,
            '/' => TOKEN::DIV,
            '\\' => TOKEN::BACKSLASH,
            '!' => TOKEN::EXCL_MARK,
            '?' => TOKEN::QUESTION_MARK,
            '.' => TOKEN::PERIOD,
            ':' => TOKEN::COLON,
            ';' => TOKEN::SEMI_COLON,
            '-' => TOKEN::SUBTRACT,
            '*' => TOKEN::MUL,
            '|' => TOKEN::PIPE,
            '^' => TOKEN::B_XOR,
            '+' => TOKEN::SUM,
            '=' => TOKEN::EQ,
            ',' => TOKEN::COMMA,
            '#' => TOKEN::MAP_OPERATOR,
            '@' => TOKEN::AT,
            '$' => TOKEN::DOLLAR,
            '%' => TOKEN::MOD,
            '~' => TOKEN::B_INV,
            '&' => TOKEN::AND,
            '_' => TOKEN::UNDERLINE,
            _ => TOKEN::_NONCE,
        };
        if matched_single != TOKEN::_NONCE || char.is_whitespace() || char == '\n' {
            save_ident();
            if char == '\n' {
                token_stream.push(TOKEN::NEWLINE.into_span(iter.current_index(), iter.current_index()));
            } else if char.is_whitespace() {
                token_stream.push(TOKEN::WHITESPACE.into_span(iter.current_index(), iter.current_index()));
            } else {
                token_stream.push(matched_single.into_span(iter.current_index(), iter.current_index()));
            }
            continue
        }
        let prev = iter.peek_prev();
        if char == '_' && prev.map_or(false, |x| x.is_ascii_digit()) == true && next.map_or(false, |x| x.is_ascii_digit()) == true {
            save_ident();
            token_stream.push(TOKEN::UNDERLINE.into_span(iter.current_index(), iter.current_index()));
            continue
        }

        current_ident.write_char(char).unwrap();
        if next.is_none() {
            let ident = current_ident.trim().to_string();
            //duped code because of mut borrow
            if ident.len() != 0 {
                token_stream.push(TOKEN::IDENTIFIER(ident).into_span((iter_count - current_ident.len() as isize) as usize, (iter_count - 1) as usize));
            }
        }
    }
    token_stream.push(TOKEN::EOF.into_span(len, len));
    let indexed_stream = fill_line_nums(token_stream, &input);
    let stage_2 = tokenize_stage_2(indexed_stream)?;
    Ok(fill_line_nums(stage_2, &input))
}

fn tokenize_stage_2(stream: Vec<TokenSpan>) -> Result<Vec<TokenSpan>, LexError> {
    let mut iter = TokenIter::wrap(stream);
    let mut tokens = vec![];
    while let Some(span) = iter.next() {
        let next = iter.peek();
        if span.token == TOKEN::DIRECTIVE_DEP {
            if next.is_none() || !next.unwrap().token.is_btick_or_reg_ident() {
                return Err(LexError::EXPECTED_IDENT(span))
            }

            let import_segments = iter.collect_and_eat(|span, next, prev| {
                let prev = prev.unwrap().token;
                if span.token.is_btick_or_reg_ident() && prev == TOKEN::DIRECTIVE_DEP || prev == TOKEN::COLON {
                    return Ok(Some((true, 0)))
                }
                if span.token == TOKEN::COLON {
                    if next.is_some() {
                        let next = next.unwrap().token;
                        if next == TOKEN::COLON {
                            return Ok(Some((false, 1)))
                        }
                    }
                    return Err(LexError::EXPECTED_ONEOF(span, vec![TOKEN::COLON]))
                }
                Ok(None)
            })?;
            if import_segments.is_empty() {
                return Err(LexError::CUSTOM(span, "WJL dependency path".into()))
            }
            let end = import_segments.last().unwrap().end;
            tokens.push(TOKEN::S2_IMPORT_PATH(import_segments).into_span(span.start, end));
            continue
        }
        if span.token == TOKEN::DIRECTIVE_EXTERNAL_DEP {
            let is_untyped = iter.peek_prev().map_or(false, |x| x.token == TOKEN::KEYWORD_UNSAFE);
            let next = iter.next();
            if next.is_none() {
                return Err(LexError::CUSTOM(span, "Node.js module name".into()))
            }
            let next = next.unwrap();
            if let TOKEN::STRING_LITERAL(module) = next.token.clone() {
                let supposed_to_be_dash = iter.next();
                let supposed_to_be_right_angled = iter.next();
                if supposed_to_be_dash.is_none() {
                    return Err(LexError::EXPECTED_ONEOF(next, vec![TOKEN::SUBTRACT]));
                }
                if supposed_to_be_right_angled.is_none() {
                    return Err(LexError::EXPECTED_ONEOF(next, vec![TOKEN::RIGHT_ANGLE]));
                }
                let supposed_to_be_dash = supposed_to_be_dash.unwrap();
                let supposed_to_be_right_angled = supposed_to_be_right_angled.unwrap();
                if supposed_to_be_dash.token == TOKEN::SUBTRACT && supposed_to_be_right_angled.token == TOKEN::RIGHT_ANGLE {
                    let receiver = iter.peek();
                    if receiver.is_none() {
                        return Err(LexError::CUSTOM(supposed_to_be_right_angled, "alias or object destructuring statement".into()))
                    }
                    let receiver = receiver.unwrap();
                    if let TOKEN::IDENTIFIER(_) = receiver.token {
                        //non destructured dependency declaration
                        let end = receiver.end;
                        tokens.push(TOKEN::S2_EXTERNAL_IMPORT {
                            module,
                            untyped: is_untyped,
                            receiver: Box::new(receiver),
                        }.into_span(span.start, end));
                        iter.next(); //drop receiver
                        continue
                    } else if receiver.token == TOKEN::LEFT_BRACE {
                        //destructured dependency declaration
                        let block = iter.parse_brace_block(true).unwrap_or(TokenSpan {
                            start: span.start,
                            end: span.start + 1,
                            token: TOKEN::S2_ARBIT_BLOCK(vec![]),
                            line_num: None,
                        });
                        let end = block.end;
                        tokens.push(TOKEN::S2_EXTERNAL_IMPORT {
                            module,
                            untyped: is_untyped,
                            receiver: Box::new(block)
                        }.into_span(span.start, end));
                        continue
                    } else {
                        return Err(LexError::EXPECTED_ONEOF(receiver, vec![TOKEN::RIGHT_BRACE]))
                    }
                } else {
                    return Err(LexError::EXPECTED_STR_GOT_TOK(supposed_to_be_dash.clone(), "->".into(), supposed_to_be_dash.token))
                }


            } else {
                return Err(LexError::CUSTOM(span, "Node.js module name".into()))

            }
        }
        if let TOKEN::NUMBER(num) = span.token {
            let negative = iter.peek_prev().map_or(false, |x|x.token == TOKEN::SUBTRACT);
            let start = span.start;
            let mut is_float = false;
            let mut base = String::new();
            let char = num.to_string().chars().next().unwrap();
            base.write_char(char).unwrap();
            let mut fraction: String = String::new();
            while let Some(span) = iter.next() {
                if span.token == TOKEN::UNDERLINE {
                    continue
                }
                if span.token == TOKEN::PERIOD {
                    is_float = true;
                    continue
                }
                if let TOKEN::NUMBER(num) = span.token {
                    let char = num.to_string().chars().next().unwrap(); //static cast of as char doesnt work here
                    if is_float {
                        fraction.write_char(char).unwrap();
                    } else {
                        base.write_char(char).unwrap();
                    }
                    continue
                }
                iter.seek(-1);
                break
            }
            tokens.push(if is_float {
                TOKEN::S2_FLOAT_LITERAL(format!("{}.{}", base, fraction).parse::<f64>().unwrap() * if negative {-1.0} else {1.0})
            } else {
                TOKEN::S2_INT_LITERAL(base.parse::<i64>().unwrap() * if negative {-1} else {1})
            }.into_span(start, iter.cursor_pos()));
            continue
        }
        // we perform the underline identifier elision here
        if next.is_none() {
            continue
        }
        let next = next.unwrap();
        if span.token.is_identifier() {
            if next.token == TOKEN::UNDERLINE {
                let mut new_ident = span.token.get_ident_name();
                new_ident.write_char('_').unwrap();
                iter.next(); //drop the underline
                let next = iter.peek();
                if next.is_some() && {
                    let uw = next.unwrap();
                    uw.token.is_identifier() || uw.token == TOKEN::UNDERLINE || uw.token.is_num()
                } {
                    while let Some(span) = iter.next() {
                        if span.token.is_btick_or_reg_ident() {
                            new_ident.write_str(span.token.get_ident_name().as_str()).unwrap();
                        } else if span.token == TOKEN::UNDERLINE {
                            new_ident.write_char('_').unwrap();
                        } else if span.token.is_num() {
                            let char = span.token.get_num().to_string().chars().next().unwrap(); //static cast of as char doesnt work here
                            new_ident.write_char(char).unwrap();
                        } else {break}
                    }
                }
                iter.seek(-1); //we seek back 1 so the last thing doesnt get dropped
                tokens.push(TOKEN::IDENTIFIER(new_ident).into_span(span.start, iter.cursor_pos() - 1));
                continue
            }
        }
        if span.token == TOKEN::UNDERLINE {
            if next.token.is_identifier() {
                let mut new_ident = String::new();
                new_ident.write_char('_').unwrap();
                while let Some(next) = iter.next() {
                    if next.token.is_identifier() {
                        new_ident.write_str(next.token.get_ident_name().as_str()).unwrap();
                        continue
                    }
                    iter.seek(-1); //keep non ident char
                    break
                }
                tokens.push(TOKEN::IDENTIFIER(new_ident).into_span(span.start, iter.cursor_pos() - 1));
                continue
            }
            if next.token == TOKEN::WHITESPACE {
                tokens.push(TOKEN::IDENTIFIER("_".to_string()).into_span(span.start, span.end));
                continue
            }
        }

        if span.token == TOKEN::WHITESPACE { //elide whitespaces
            let next = iter.peek();
            if next.is_some() && next.unwrap().token == TOKEN::WHITESPACE {
                while let Some(maybe_white_space) = iter.next() {
                    if maybe_white_space.token == TOKEN::WHITESPACE {
                        continue
                    } else {
                        iter.seek(-1);
                        break
                    }
                }
            }
            tokens.push(TOKEN::WHITESPACE.into_span(span.start, iter.cursor_pos()));
            continue
        }

        if span.token == TOKEN::NEWLINE {
            tokens.push(TOKEN::SEMI_COLON.into_span(span.start, span.end)); // so we dont have to check for ln break OR semicolon in the parsing stage
            continue
        }
        let next_2 = iter.peek_n(2); // we borrow the n+2 peek here so the closure doesnt fuck up the mutability, but its only used in the down below codeblock
        {
            let mut push_t = |t: TOKEN| {
                tokens.push(t.into_span(span.start, next.end));
                iter.next();
            };
            if next.token == TOKEN::COLON {
                match span.token {
                    TOKEN::QUESTION_MARK => { push_t(TOKEN::S2_ELVIS); continue },
                    TOKEN::EXCL_MARK => {push_t(TOKEN::S2_LEVIS); continue},
                    TOKEN::COLON => {push_t(TOKEN::S2_PATH_SEGMENT); continue}
                    _ => {}
                }
            }
            if span.token == TOKEN::EQ {
                match next.token {
                    TOKEN::EQ => {push_t(TOKEN::S2_EQ); continue},
                    TOKEN::MUL => {push_t(TOKEN::S2_REASS_MUL); continue},
                    TOKEN::DIV => {push_t(TOKEN::S2_REASS_DIV); continue},
                    TOKEN::SUBTRACT => {push_t(TOKEN::S2_REASS_SUB); continue},
                    TOKEN::SUM => {push_t(TOKEN::S2_REASS_ADD); continue},
                    _ => {
                        tokens.push(TOKEN::S2_ASSIGN.into_span(span.start, span.end));
                        continue
                    }
                }
            }
            if span.token == TOKEN::SUM && next.token == TOKEN::SUM {
                push_t(TOKEN::S2_INCR);
                continue
            } else if span.token == TOKEN::SUBTRACT && next.token == TOKEN::SUBTRACT {
                push_t(TOKEN::S2_DECR);
                continue
            }
            if span.token == TOKEN::MUL && next.token == TOKEN::MUL {
                push_t(TOKEN::S2_POW);
                continue
            }

            // we dont parse R_GT or L_GT cuz of generic arg possibility
            if span.token == TOKEN::LEFT_ANGLE { // <
                if next.token == TOKEN::EQ { // <=
                    push_t(TOKEN::S2_R_GTE);
                    continue
                }
                if next.token == TOKEN::LEFT_ANGLE { // <<
                    push_t(TOKEN::S2_B_LEFT_FILL);
                    continue
                }
            }

            if span.token == TOKEN::PIPE && next.token == TOKEN::RIGHT_ANGLE { // |>
                push_t(TOKEN::S2_PIPE_OP);
                continue
            }
            if span.token == TOKEN::PIPE && next.token == TOKEN::PIPE {
                push_t(TOKEN::S2_OR);
                continue
            }

            if span.token == TOKEN::RIGHT_ANGLE { // >
                if next.token == TOKEN::EQ { // >=
                    push_t(TOKEN::S2_L_GTE);
                    continue
                }

                if next.token == TOKEN::RIGHT_ANGLE { // >>
                    if next_2.is_some() && next_2.unwrap().token == TOKEN::RIGHT_ANGLE { // >>>
                        iter.next(); //drop both
                        let end = iter.next().unwrap().end;
                        tokens.push(TOKEN::S2_B_RIGHT_FILL.into_span(span.start, end));
                        continue
                    } else { // >>
                        push_t(TOKEN::S2_B_SIGNED_RIGHT);
                        continue
                    }
                }
            }
            if span.token == TOKEN::PERIOD && next.token == TOKEN::PERIOD && next_2.is_some() && next_2.unwrap().token == TOKEN::PERIOD {
                iter.next(); //drop both
                let end = iter.next().unwrap().end;
                tokens.push(TOKEN::S2_SPREAD_OP.into_span(span.start, end));
                continue
            }
            if span.token == TOKEN::AND {
                if next.token == TOKEN::AND {
                    push_t(TOKEN::S2_AND);
                    continue
                }
                tokens.push(TOKEN::S2_B_AND.into_span(span.start, span.end));
                continue
            }

            if span.token == TOKEN::DOLLAR && next.token.is_btick_or_reg_ident() {
                push_t(TOKEN::S2_LABEL(next.token.get_ident_name()));
                continue
            }


        }

        tokens.push(span)
    }
    return Ok(tokens)
}