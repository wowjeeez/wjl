use crate::lexer::tokens::TOKEN;


#[derive(Debug, Clone, PartialEq)]
pub struct TokenSpan {
    pub start: usize,
    pub end: usize,
    pub token: TOKEN,
    pub line_num: Option<(usize, usize)>
}

impl TOKEN {
    pub fn into_span(self, start: usize, end: usize) -> TokenSpan {
        TokenSpan {
            token: self,
            start,
            end,
            line_num: None
        }
    }

}

impl TokenSpan {
    pub fn get_codeblock_content(&self) -> Vec<TokenSpan> {
        match &self.token {
            TOKEN::S2_ARBIT_BLOCK(inner) => inner.clone(),
            _ => vec![]
        }
    }
}

fn line_number_from_position(input: &str, pos: usize) -> (usize, usize) {
    let mut line = 1;
    let mut col = 1;
    let mut last_newline_pos = 0;

    for (i, char) in input.char_indices() {
        if i >= pos {
            break;
        }
        if char == '\n' {
            line += 1;
            last_newline_pos = i + 1;
        }
    }

    col = pos - last_newline_pos + 1;
    (line, col)
}

pub fn fill_line_nums(arr: Vec<TokenSpan>, buf: &String) -> Vec<TokenSpan> {
    arr.into_iter().map(|span| TokenSpan {
        start: span.start,
        end: span.end,
        token: span.token,
        line_num: Some(line_number_from_position(buf.as_str(), span.start)),
    }).collect::<Vec<TokenSpan>>()
}