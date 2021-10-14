#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct Pos {
    pub lineno: usize,
    pub colno: usize,
    pub idx: usize,
}

impl Pos {
    pub fn new(lineno: usize, colno: usize, idx: usize) -> Pos {
        Pos { lineno, colno, idx }
    }

    pub fn move_next_idx(&mut self)  {
        self.idx += 1;
    }

    pub fn move_next_pos(&mut self)  {
        self.colno += 1;
        self.idx += 1;
    }

    pub fn move_next_line(&mut self)  {
        self.lineno += 1;
        self.colno = 0;
        self.idx += 1;
    }

    pub fn get_next_pos(&self) -> Pos {
        Pos {
            lineno: self.lineno,
            colno: self.colno + 1,
            idx: self.idx + 1
        }
    }

    pub const ZERO: Pos = Pos {
        lineno: 0,
        colno: 0,
        idx: 0
    };

    pub const MAX: Pos = Pos {
        lineno: usize::MAX,
        colno: usize::MAX,
        idx: usize::MAX
    };
}

impl PartialOrd for Pos {
    fn partial_cmp(&self, other: &Pos) -> Option<std::cmp::Ordering> {
        Some(self.idx.cmp(&other.idx))
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub struct Span {
    pub start: Pos,
    pub end: Pos,
}