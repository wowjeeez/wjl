use crate::errors::WjlError;
use crate::helpers::find_line_number_and_char_position;

#[derive(Debug)]
pub struct ErrorReporter<'a> {
    pub(crate) file: &'a String,
    pub(crate) file_name: String,
    pub(crate) errors: Vec<WjlError>,
}

impl ErrorReporter<'_> {
    pub fn add<T: Into<WjlError>>(&mut self, error: T) {
        let mut error = error.into();
        let (line_pos, char_pos) = find_line_number_and_char_position(&self.file, error.char);
        if error.end_char.is_some() {
            let (end_l_pos, end_c_pos) =
                find_line_number_and_char_position(&self.file, error.end_char.unwrap());
            error._set_loc_ranged(line_pos, char_pos, end_l_pos, end_c_pos);
        } else {
            error._set_loc(line_pos, char_pos);
        }
        self.errors.push(error.into());
    }

    pub fn for_file(file_name: String, content: &String) -> ErrorReporter {
        ErrorReporter {
            file_name,
            file: content,
            errors: vec![],
        }
    }

    pub fn is_ok(&self) -> bool {
        self.errors.len() == 0
    }
}
