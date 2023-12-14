use std::fs::{File, OpenOptions};
use std::io;
use std::io::{BufRead, BufReader, Read, Seek};
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

impl StreamBuffer<File> {
    pub fn from_file(f: &str) -> io::Result<Self> {
        let f = OpenOptions::new().read(true).open(f)?;

        Ok(Self {
            reader: BufReader::new(f),
        })
    }
}

impl<R: Read + Seek> Buffer for StreamBuffer<R> {
    fn next(&mut self) -> (usize, Option<char>) {
        // get current position
        let pos = self.reader.stream_position().unwrap() as usize;

        // get buffer
        let mut buf = self.reader.buffer();
        if buf.is_empty() {
            match self.reader.fill_buf() {
                Ok(b) if !b.is_empty() => buf = b,
                _ => return (0, None),
            }
        }

        let r = (pos, Some(buf[0] as char));

        self.reader.consume(1);
        r
    }

    fn stage(&mut self, stage: (usize, Option<char>)) {
        match stage {
            (_, None) => {}
            (_, Some(c)) => {
                self.reader.seek_relative(-1).unwrap();
            }
        }
    }
}
