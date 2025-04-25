use std::fmt::Debug;
#[derive(Debug)]
pub struct Span<T> {
    start_index: usize,
    end_index: usize,
    content: T,
}

impl <T> Span<T> {
    pub fn new(start_index: usize, end_index: usize, content: T) -> Span<T> {
        Span {
            start_index,
            content,
            end_index,
        }
    }

    pub fn get(&self) -> &T {
        &self.content
    }

    pub fn start(&self) -> usize {
        self.start_index
    }

    pub fn end(&self) -> usize {
        self.end_index
    }

    pub fn size(&self) -> usize {
        self.end_index - self.start_index
    }
}


impl <T: Clone> Span<T> {
    pub fn get_cloned(&self) -> T {
        self.content.clone()
    }
}

impl <T: Clone>Clone for Span<T> {
    fn clone(&self) -> Self {
        Span {
            start_index: self.start_index,
            end_index: self.end_index,
            content: self.content.clone(),
        }
    }
}