use std::{
    iter::{Iterator, Peekable},
    rc::Rc,
};
use super::{
    span::{Pos, Span},
    token::{TokenType, Token},
    err::LexError
};

macro_rules! next_if_ch_eq {
    ($self:expr, $ch:expr) => {
        $self.next_if(|(_, c)| *c == $ch).is_some()
    };
}

struct StringIter<T>
    where T: Iterator<Item = char>,
{
    chars: std::iter::Chain<T, std::iter::Once<char>>,
    pos: Pos,
    is_last_cr: bool
}

impl<T> StringIter<T>
    where T: Iterator<Item = char>,
{
    pub fn new(src: T) -> StringIter<T> {
        StringIter {
            chars: src.chain(std::iter::once('\0')),
            pos: Pos::ZERO,
            is_last_cr: false,
        }
    }
}

impl<T> Iterator for StringIter<T>
    where T: Iterator<Item = char>,
{
    type Item = (Pos, char);

    fn next(&mut self) -> Option<Self::Item> {
        match self.chars.next() {
            Some(c) => {
                let ret = Some((self.pos, c));
                match c {
                    '\n' => {
                        if !self.is_last_cr {
                            self.pos.move_next_line();
                        } else {
                            self.pos.move_next_idx();
                        }
                        self.is_last_cr = false;
                    }
                    '\r' => {
                        self.pos.move_next_line();
                        self.is_last_cr = true;
                    }
                    _ => {
                        self.is_last_cr = false;
                        self.pos.move_next_pos();
                    }
                };
                ret
            }
            None => None
        }
    }
}

pub struct Lexer<T>
    where T: Iterator<Item = char>,
{
    iter: Peekable<StringIter<T>>,
    err: Option<Vec<Rc<LexError>>>,
}

type LexResult = Result<Token, LexError>;

impl<T> Iterator for Lexer<T>
    where T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            let token = self.next_token();

            // TODO: let-else
            if let Some(Token { token_type: TokenType::Comment(..), .. }) = token {
                continue;
            } else {
                break token;
            }
        }
    }
}

impl<T> Lexer<T>
    where T: Iterator<Item = char>,
{
    pub fn new(iter: T) -> Lexer<T> {
        Lexer {
            iter: StringIter::new(iter).peekable(),
            err: None,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_spaces();
        let (start, c) = match self.iter.peek() {
            None | Some((_, '\0')) => return None,
            Some((pos, c)) => (*pos, *c),
        };

        let token_result = match c {
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_keyword(),
            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '!' | '|' | '&' | '^' | '(' | ')' | '['
            | ']' | '{' | '}' | ',' | ';' => self.lex_operator(),
            c @ _ => Err(LexError::UnexpectedCharacter(c)),
        };

        let token = match token_result {
            Ok(token) => token,
            Err(e) => {
                let end = self.skip_error_token();
                let e = Rc::new(e);
                match &mut self.err {
                    Some(v) => v.push(e.clone()),
                    None => self.err = Some(vec![e.clone()])
                }
                Token {
                    token_type: TokenType::Err(e.clone()),
                    span: Span { start, end },
                }
            }
        };

        Some(token)
    }

    fn skip_error_token(&mut self) -> Pos {
        loop {
            if self.iter.next_if(|(_, c)| {
                c.is_whitespace() || *c == '\0'
            }).is_some() {
                break;
            }
        }
        self.iter.peek().map_or(Pos::MAX, |(pos, _)| *pos)
    }

    fn lex_number(&mut self) -> LexResult {
        let start = self.iter.peek().unwrap().0;

        let radix = if next_if_ch_eq!(self.iter, '0') {
            if next_if_ch_eq!(self.iter, 'x') { 16 } else { 8 }
        } else {
            10
        };

        let mut number = String::from("0");
        while let Some((_, c)) = self.iter.next_if(|(_, c)| c.is_digit(radix)) {
            number.push(c);
        }

        let end = self.iter.peek().unwrap().0;

        match i32::from_str_radix(&number, radix) {
            Ok(i) => Ok(Token {
                token_type: TokenType::IntLiteral(i),
                span: Span { start, end }
            }),
            Err(_) => Err(LexError::IllegalLiteral)
        }
    }

    fn lex_identifier_keyword(&mut self) -> LexResult {
        let start = self.iter.peek().unwrap().0;

        let mut ident = String::new();
        while let Some((_, c)) = self.iter.next_if(|(_, c)| {
            c.is_alphanumeric() || *c == '_'
        }) {
            ident.push(c);
        }

        let end = self.iter.peek().unwrap().0;

        let token_type = match &ident[..] {
            "const" => TokenType::ConstKw,
            "break" => TokenType::BreakKw,
            "continue" => TokenType::ContinueKw,
            "if" => TokenType::IfKw,
            "else" => TokenType::ElseKw,
            "while" => TokenType::WhileKw,
            "return" => TokenType::ReturnKw,
            _ => TokenType::Ident(ident),
        };
        Ok(Token{
            token_type,
            span: Span { start, end }
        })
    }

    fn lex_operator(&mut self) -> LexResult {
        let (start, first_char) = self.iter.next().unwrap();

        if first_char == '/' {
            if next_if_ch_eq!(self.iter, '*') {
                return self.lex_comments(true);
            } else if next_if_ch_eq!(self.iter, '/') {
                return self.lex_comments(false);
            }
        }

        let token_type = match first_char {
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            '*' => TokenType::Mul,
            '/' => TokenType::Div,
            '%' => TokenType::Mod,
            '=' => if next_if_ch_eq!(self.iter, '=') {
                TokenType::Eq
            } else {
                TokenType::Assign
            }
            '<' => if next_if_ch_eq!(self.iter, '=') {
                TokenType::Le
            } else {
                TokenType::Lt
            }
            '>' => if next_if_ch_eq!(self.iter, '=') {
                TokenType::Ge
            } else {
                TokenType::Gt
            }
            '!' => if next_if_ch_eq!(self.iter, '=') {
                TokenType::Ne
            } else {
                TokenType::Not
            }
            '|' => if next_if_ch_eq!(self.iter, '|') {
                TokenType::Or
            } else {
                return Err(LexError::UnexpectedCharacter('|'))
            }
            '&' => if next_if_ch_eq!(self.iter, '&') {
                TokenType::And
            } else {
                return Err(LexError::UnexpectedCharacter('&'))
            }
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            ',' => TokenType::Comma,
            ';' => TokenType::Semicolon,
            _ => unreachable!(),
        };

        let end = self.iter.peek().unwrap().0;

        Ok(Token{
            token_type,
            span: Span { start, end }
        })
    }

    fn lex_comments(&mut self, multi_line: bool) -> LexResult {
        let mut comment = String::new();
        let start = self.iter.peek().unwrap().0;

        if multi_line {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '*')) if next_if_ch_eq!(self.iter, '/') => break,
                    Some((_, c)) => comment.push(c),
                    None => return Err(LexError::UnexpectedEOF)
                }
            }
        } else {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '\r')) | Some((_, '\n')) | Some((_, '\0')) => break,
                    Some((_, c)) => comment.push(c),
                    None => break,
                }
            }
        }

        let end = self.iter.peek().unwrap().0;
        Ok(Token {
            token_type: TokenType::Comment(comment),
            span: Span { start, end }
        })
    }

    fn skip_spaces(&mut self) {
        loop {
            if self.iter.next_if(|(_, c)| {
                *c != '\0' && c.is_whitespace()
            }).is_none() {
                break;
            }
        }
    }
}
