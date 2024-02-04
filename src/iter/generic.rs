pub trait GenericIterator<T: Clone> {
    fn get_index(&self) -> Option<usize>;
    fn set_index(&mut self, to: usize);

    fn get_content(&self) -> &Vec<T>;

    fn next(&mut self) -> Option<T> {
        let curr = self.get_index().map_or(0, |x| x + 1);
        self.set_index(curr);
        self.get_content().get(curr).map(|x| x.clone())
    }

    fn peek_next(&self) -> Option<T> {
        self.peek_n(1)
    }
    fn peek_n(&self, by: isize) -> Option<T> {
        let curr = self.get_index().map_or(0_isize + by, |x| x as isize + by);
        self.get_content()
            .get(if curr < 0 { 0 } else { curr as usize })
            .map(|x| x.clone()) //prevent panic
    }

    fn seek(&mut self, by: isize) {
        let curr = self.get_index().map_or(0_isize + by, |x| x as isize + by);
        self.set_index(if curr < 0 { 0 } else { curr as usize });
    }

    fn curr(&self) -> Option<T> {
        self.peek_n(0)
    }

    fn peek_prev(&self) -> Option<T> {
        self.peek_n(-1)
    }
}

pub struct PeekableIterator<T> {
    inner: Vec<T>,
    index: Option<usize>,
}

impl<T: Clone> GenericIterator<T> for PeekableIterator<T> {
    fn get_index(&self) -> Option<usize> {
        self.index
    }

    fn set_index(&mut self, to: usize) {
        self.index = Some(to)
    }

    fn get_content(&self) -> &Vec<T> {
        &self.inner
    }
}

pub fn wrap_iter<T>(elements: Vec<T>) -> PeekableIterator<T> {
    PeekableIterator {
        inner: elements,
        index: None,
    }
}
