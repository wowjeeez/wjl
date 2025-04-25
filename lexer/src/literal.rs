use common::iter::SmartIterator;

pub fn lex_interpolated(interpolated: String) {
    let mut stream = SmartIterator::new(interpolated.chars().collect());
    
}