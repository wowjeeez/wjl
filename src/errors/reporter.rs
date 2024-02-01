use crate::errors::WjlError;

#[derive(Debug)]
pub struct ErrorReporter<'a> {
    file: &'a String,
    file_name: String,
    errors: Vec<WjlError>
}

impl ErrorReporter<'_> {
    pub fn add<T: Into<WjlError>>(&mut self, error: T) {
        let mut error = error.into();
        let (line_pos, char_pos) = self.find_line_number_and_char_position(error.char);
        if error.end_char.is_some() {
            let (end_l_pos, end_c_pos) = self.find_line_number_and_char_position(error.end_char.unwrap());
            error._set_loc_ranged(line_pos, char_pos, end_l_pos, end_c_pos);
        } else {
            error._set_loc(line_pos, char_pos);
        }
        self.errors.push(error.into());
    }
    fn find_line_number_and_char_position(&self, char_index: usize) -> (usize, usize) {
        if char_index > self.file.len() {
            return (0, 0);
        }

        let mut line_number = 1;
        let mut char_position = 1;
        let mut current_index = 0;

        for c in self.file.chars() {
            if current_index == char_index {
                return (line_number, char_position);
            }
            if c == '\n' {
                line_number += 1;
                char_position = 0;
            }
            current_index += c.len_utf8();
            char_position += 1;
        }

        if current_index == char_index {
            return (line_number, char_position);
        }

        (0, 0)
    }

    pub fn for_file(file_name: String, content: &String) -> ErrorReporter {
        ErrorReporter {
            file_name,
            file: content,
            errors: vec![]
        }
    }
}