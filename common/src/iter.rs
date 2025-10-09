use crate::Location;

pub struct SmartIterator<T> {
    inner: Vec<T>,
    index: Option<usize>,
}

impl<T> SmartIterator<T> {
    pub fn new(input: Vec<T>) -> SmartIterator<T> {
        SmartIterator {
            inner: input,
            index: None,
        }
    }

    pub fn seek(&mut self, to: usize) {
        self.index = Some(to);
    }
    
    pub fn next(&mut self) -> Option<(&T, usize)> {
        let new_index = self.get_next_index();
        let element = self.inner.get(new_index);
        self.index = Some(new_index);
        element.map(|e| (e, new_index))
    }
    
    pub fn get_next_index(&self) -> usize {
        self.index.map_or(0,  |i| i + 1)
    }
    
    pub fn get_current_index(&self) -> usize {
        self.index.unwrap_or(0)
    }
    
    pub fn peek(&self) -> Option<&T> {
        let new_index = self.get_next_index();
        self.inner.get(new_index)
    }
    
    pub fn get_previous_index(&self) -> usize {
        self.get_current_index().checked_sub(1).unwrap_or(0)
    }

    pub fn lookbehind(&self) -> Option<&T> {
        let new_index = self.get_previous_index();
        self.inner.get(new_index)
    }
}

impl<T: PartialEq> SmartIterator<T> {
    pub fn lookbehind_is(&self, rhs: &T) -> bool {
        self.lookbehind_is_fn(|t| t == rhs)
    }

    pub fn lookbehind_is_fn<E: FnOnce(&T) -> bool>(&self, rhs: E) -> bool {
        self.lookbehind().map_or(false, rhs)
    }

    pub fn peek_is(&self, rhs: &T) -> bool {
        self.peek_is_fn(|t| t == rhs)
    }
    
    pub fn peek_is_fn<E: FnOnce(&T) -> bool>(&self, rhs: E) -> bool {
        self.peek().map_or(false, rhs)
    }

    pub fn curr(&self) -> Option<&T> {
        self.inner.get(self.get_current_index())
    }
}