use std::fmt::Write;

pub fn find_line_number_and_char_position(file: &String, char_index: usize) -> (usize, usize) {
    if char_index > file.len() {
        return (0, 0);
    }

    let mut line_number = 1;
    let mut char_position = 1;
    let mut current_index = 0;

    for c in file.chars() {
        if current_index == char_index {
            return (line_number, char_position);
        }
        if c as u8 == 0x0A {
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

pub fn lineify(file: &String) -> Vec<String> {
    let mut lines = vec![];
    let mut line = String::new();
    for c in file.chars() {
        if c as u8 == 0x0A {
            lines.push(line.clone());
            line.clear();
            continue
        }
        line.write_char(c).unwrap()
    }
    lines.push(line.clone());
    line.clear();
    lines
}