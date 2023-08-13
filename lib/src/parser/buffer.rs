use std::io::BufReader;
use std::str::CharIndices;

pub trait Buffer {
    fn next(&mut self) -> (usize, Option<char>);
    fn stage(&mut self, stage: (usize, Option<char>));
}

pub struct StringBuffer<'input> {
    chars: CharIndices<'input>,
    len: usize,
    stage: Option<(usize, Option<char>)>,
}

impl Buffer for StringBuffer<'_> {
    fn next(&mut self) -> (usize, Option<char>) {
        if let Some(r) = self.stage.take() {
            return r;
        }

        match self.chars.next() {
            Some((index, ch)) => (index, Some(ch)),
            _ => (self.len, None),
        }
    }

    fn stage(&mut self, stage: (usize, Option<char>)) {
        self.stage = Some(stage);
    }
}

impl<'input> StringBuffer<'input> {
    pub fn new(input: &'input str) -> Self {
        Self {
            chars: input.char_indices(),
            len: input.len(),
            stage: None,
        }
    }
}

pub struct StreamBuffer<R> {
    reader: BufReader<R>,
}

impl<R> Buffer for StreamBuffer<R> {
    fn next(&mut self) -> (usize, Option<char>) {
        todo!()
    }

    fn stage(&mut self, stage: (usize, Option<char>)) {
        todo!()
    }
}
