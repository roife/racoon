#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Span {
    pub idx: usize,
    pub len: usize,
}

impl Span {
    pub fn start(&self) -> usize {
        self.idx
    }

    pub fn end(&self) -> usize {
        self.idx + self.len
    }

    pub fn new(idx: usize, len: usize) -> Span {
        Span { idx, len }
    }

    pub fn from_range(start: usize, end: usize) -> Span {
        println!("[{}, {}]", start, end);
        assert!(start < end);
        Span { idx: start, len: end - start }
    }
}