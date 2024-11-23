use std::collections::VecDeque;
use std::fs::{File, OpenOptions};
use std::io;
use std::io::{BufRead, BufReader, Read, Seek};

pub trait Buffer {
    /// Consume 1 char, panic if no character left in the buffer
    fn consume1(&mut self);
    /// Peek at the next character
    #[inline]
    fn peek1(&mut self) -> Option<char> {
        self.peek(1)
    }
    /// Peek character at 'n' position from current
    fn peek(&mut self, n: usize) -> Option<char>;
    fn current_line(&self) -> usize;
    fn line_offset(&self) -> usize;
    fn buffer_offset(&mut self) -> usize;
}

pub struct IterBuffer<'str> {
    iter: Box<dyn Iterator<Item = char> + 'str>,
    peek_buffer: VecDeque<char>,
    current_line: usize,
    line_offset: usize,
    buffer_offset: usize,
}

impl<'str> IterBuffer<'str> {
    pub fn new<T: Iterator<Item = char> + 'str>(iter: T) -> Self {
        Self {
            iter: Box::new(iter),
            peek_buffer: VecDeque::with_capacity(1024),
            current_line: 0,
            line_offset: 0,
            buffer_offset: 0,
        }
    }
}

impl Buffer for IterBuffer<'_> {
    fn consume1(&mut self) {
        let c = if !self.peek_buffer.is_empty() {
            self.peek_buffer.pop_front()
        } else {
            self.iter.next()
        };

        match c {
            None => {}
            Some('\r') | Some('\n') => {
                self.buffer_offset += 1;
                self.line_offset = 0;
                self.current_line += 1;
                let br = c == Some('\n');

                // Extra break line character eat
                match (br, self.peek1()) {
                    (true, Some('\r')) | (false, Some('\n')) => {
                        self.buffer_offset += 1;

                        if !self.peek_buffer.is_empty() {
                            self.peek_buffer.pop_front()
                        } else {
                            self.iter.next()
                        };
                    }
                    _ => {}
                }
            }
            _ => {
                self.line_offset += 1;
                self.buffer_offset += 1;
            }
        };
    }

    fn peek(&mut self, n: usize) -> Option<char> {
        assert!(n > 0);

        // for optimize
        let index = n - 1;
        if self.peek_buffer.len() > index {
            return Some(self.peek_buffer[index]);
        }

        // fill buffer
        let remain_buffer = self.peek_buffer.capacity() - self.peek_buffer.len();
        for _ in 0..remain_buffer {
            match self.iter.next() {
                Some(c) => self.peek_buffer.push_back(c),
                None => break,
            }
        }

        // get result
        if self.peek_buffer.len() > index {
            Some(self.peek_buffer[index])
        } else {
            None
        }
    }

    fn current_line(&self) -> usize {
        self.current_line
    }

    fn line_offset(&self) -> usize {
        self.line_offset
    }

    fn buffer_offset(&mut self) -> usize {
        self.buffer_offset
    }
}

pub struct StreamBuffer<R> {
    reader: BufReader<R>,
    current_line: usize,
    current_offset: usize,
}

impl StreamBuffer<File> {
    pub fn from_file(f: &str) -> io::Result<Self> {
        let f = OpenOptions::new().read(true).open(f)?;

        Ok(Self {
            // 4k capacity
            reader: BufReader::with_capacity(4 * 1024, f),
            current_line: 0,
            current_offset: 0,
        })
    }
}

impl<R: Read + Seek> Buffer for StreamBuffer<R> {
    fn consume1(&mut self) {
        match self.peek1() {
            Some('\r') => {
                self.reader.consume(1);
                if let Some('\n') = self.peek1() {
                    self.reader.consume(1);
                }

                self.current_line += 1;
                self.current_offset = 0;
            }
            Some('\n') => {
                self.reader.consume(1);
                if let Some('\r') = self.peek1() {
                    self.reader.consume(1);
                }

                self.current_line += 1;
                self.current_offset = 0;
            }
            Some(_) => {
                self.reader.consume(1);

                self.current_offset += 1;
            }
            None => panic!(),
        }
    }

    fn peek(&mut self, n: usize) -> Option<char> {
        assert!(n > 0);

        let index = n - 1;
        let mut buf = self.reader.buffer();
        if buf.len() <= index {
            match self.reader.fill_buf() {
                Ok(b) if b.len() > index => buf = b,
                _ => return None,
            }
        }

        Some(buf[index] as char)
    }

    fn current_line(&self) -> usize {
        self.current_line
    }

    #[inline]
    fn line_offset(&self) -> usize {
        self.current_offset
    }

    #[inline]
    fn buffer_offset(&mut self) -> usize {
        self.reader.stream_position().unwrap() as usize
    }
}
