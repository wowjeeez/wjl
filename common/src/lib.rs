pub mod span;
pub mod iter;

pub use tokio;
pub use either;
use thiserror::Error;
use crate::span::Span;

#[derive(Error, Debug)]
pub struct Location<T> {
    pub start: Span<T>,
    pub end: Option<Span<T>>,
}

impl<T> Location<T> {
    pub fn with_end(mut self, end: Span<T>) -> Self {
        self.end = Some(end);
        self
    }

    pub fn wrap(start: Span<T>) -> Location<T> {
        Location { start, end: None }
    }
}

pub trait LocationDataProvider<T> {
    fn get_location(&self) -> Location<T>;
}