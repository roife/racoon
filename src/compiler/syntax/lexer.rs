use std::{
    iter::{Iterator, Peekable},
};
use std::iter::Enumerate;
use super::{
    span::Span,
    token::{TokenType, Token},
    err::LexError
};

pub struct Lexer<T>
    where T: Iterator<Item = char>,
{
    iter: Peekable<Enumerate<T>>,
    err: Option<Vec<LexError>>,
}

type LexResult = Result<Token, LexError>;

impl<T> Iterator for Lexer<T>
    where T: Iterator<Item = char>,
{
    type Item = Token;

    fn next(&mut self) -> Option<Token> {
        loop {
            let token = self.next_token();

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
            iter: iter.enumerate().into_iter().peekable(),
            err: None,
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        self.skip_spaces();
        let (start, c) = match self.iter.peek() {
            None | Some((_, '\0')) => return None,
            Some(&(pos, c)) => (pos, c),
        };

        let token = match c {
            '0'..='9' => self.lex_number(),
            'a'..='z' | 'A'..='Z' | '_' => self.lex_identifier_keyword(),
            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' | '!' | '|' | '&' | '^' | '(' | ')' | '['
            | ']' | '{' | '}' | ',' | ';' => self.lex_operator(),
            _ => Err(LexError::None),
        };

        let token = match token {
            Ok(t) => t,
            Err(e) => {
                let end = self.skip_error_token();
                match &mut self.err {
                    Some(vec) => vec.push(e),
                    None => self.err = Some(vec![e]),
                }
                Token {
                    token_type: TokenType::Err(e),
                    span: Span::from_range(start, end),
                }
            }
        };

        Some(token)
    }

    fn skip_error_token(&mut self) -> usize {
        // TODO
        loop {
            if self.iter.peek().map_or(true, |(_, c)| c.is_whitespace()) {
                break;
            }
            self.iter.next();
        }
        self.iter.peek().map_or(0, |(pos, _)| *pos)
    }

    fn lex_number(&mut self) -> LexResult {
        let start = self.iter.peek().expect("Start pos not valid").0;

        let radix = if self.iter.next_if(|(_, c)| c == '0').is_some() {
            if self.iter.next_if(|(_, c)| c == 'x').is_some() {
                16
            } else {
                8
            }
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
                span: Span::from_range(start, end)
            }),
            Err(_) => Err(LexError::None)
        }
    }

    fn lex_identifier_keyword(&mut self) -> LexResult {
        let start = self.iter.peek().expect("Start pos not valid").0;

        let mut ident = String::new();
        while let Some((_, c)) = self.iter.next_if(|(_, c)| c.is_alphanumeric() || c == '_') {
            ident.push(c);
        }

        let end = self.iter.peek().unwrap().0;

        let token_type = match &ident[..] {
            "const" => TokenType::ConstKw,
            "int" => TokenType::Int,
            "void" => TokenType::Void,
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
            span: Span::from_range(start, end)
        })
    }

    fn lex_operator(&mut self) -> LexResult {
        let (start, first_char) = self.iter.next().expect("Start pos not valid");
        let second_char = match self.iter.peek() {
            Some((_, c)) => Some(c),
            None => None
        };
        let len = if second_char.is_some() { 2 } else { 1 };

        if first_char == '/' {
            match second_char {
                Some('*') => {
                    self.iter.next();
                    return self.lex_comments(true);
                },
                Some('/') => {
                    self.iter.next();
                    return self.lex_comments(false);
                },
                _ => {}
            }
        }

        let token_type = match first_char {
            '+' => TokenType::Plus,
            '-' => TokenType::Minus,
            '*' => TokenType::Mul,
            '/' => match second_char {
                Some('*') | Some('/') => unreachable!(),
                _ => TokenType::Div
            },
            '%' => TokenType::Mod,
            '=' => match second_char {
                Some('=') => {
                    self.iter.next();
                    TokenType::Eq
                },
                _ => TokenType::Assign,
            },
            '<' => match second_char {
                Some('=') => {
                    self.iter.next();
                    TokenType::Le
                },
                _ => TokenType::Lt,
            },
            '>' => match second_char {
                Some('=') => {
                    self.iter.next();
                    TokenType::Ge
                },
                _ => TokenType::Gt,
            },
            '!' => match second_char {
                Some('=') => {
                    self.iter.next();
                    TokenType::Ne
                },
                _ => TokenType::Not,
            },
            '|' => match second_char {
                Some('|') => {
                    self.iter.next();
                    TokenType::Or
                },
                _ => return Err(LexError::None)
            },
            '&' => match second_char {
                Some('&') => {
                    self.iter.next();
                    TokenType::And
                },
                _ => return Err(LexError::None)
            },
            '(' => TokenType::LParen,
            ')' => TokenType::RParen,
            '[' => TokenType::LBracket,
            ']' => TokenType::RBracket,
            '{' => TokenType::LBrace,
            '}' => TokenType::RBrace,
            ',' => TokenType::Comma,
            ';' => TokenType::Semicolon,
            _ => return Err(LexError::None),
        };

        Ok(Token{
            token_type,
            span: Span::new(start, len)
        })
    }

    fn lex_comments(&mut self, multi_line: bool) -> LexResult {
        let mut comment = String::new();
        let start = self.iter.peek().unwrap().0;
        let mut end = start + 1;

        if multi_line {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '*')) if match self.iter.peek()  {
                        Some((_, '/')) => true,
                        _ => false
                    } => {
                        self.iter.next();
                        break;
                    }
                    Some((_, c)) => comment.push(c),
                    None => Err(LexError::None)?
                }
                end += 1;
            }
        } else {
            loop {
                let c = self.iter.next();
                match c {
                    Some((_, '\r')) | Some((_, '\n')) | Some((_, '\0')) => {
                        self.iter.next();
                        break;
                    }
                    Some((_, c)) => comment.push(c),
                    None => break,
                }
                end += 1;
            }
        }
        Ok(Token {
            token_type: TokenType::Comment(comment),
            span: Span::from_range(start, end)
        })
    }

    fn skip_spaces(&mut self) {
        while match self.iter.peek() {
            Some(&(_, c)) => c != '\0' && c.is_whitespace(),
            None => false,
        } {
            self.iter.next();
        }
    }
}