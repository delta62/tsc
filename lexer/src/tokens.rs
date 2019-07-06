use std::iter::{Enumerate,Peekable};
use std::str::Chars;

use super::charclass::is_line_terminator;
use super::errors::*;
use super::token::Token;
use super::tokentype::TokenType;

pub struct Tokens<'a> {
    input: Peekable<Enumerate<Chars<'a>>>,
    diagnostics: Vec<Error>,
}

impl <'a> Tokens<'a> {
    pub fn new(input: &'a str) -> Tokens<'a> {
        let stream = input.chars().enumerate().peekable();
        Tokens {
            input: stream,
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &Vec<Error> {
        &self.diagnostics
    }

    fn unexpected_char(idx: usize, c: char) -> Result<Token> {
        Err(ErrorKind::UnexpectedChar(c, idx).into())
    }

    fn unexpected_eof() -> Result<Token> {
        Err(ErrorKind::UnexpectedEof.into())
    }

    fn skip_yield(&mut self, result: TokenType) -> TokenType {
        self.input.next();
        result
    }

    fn peek_char(&mut self) -> Option<char> {
        match self.input.peek() {
            Some((_, c)) => Some(*c),
            None         => None,
        }
    }

    fn skip_then_peek(&mut self) -> Option<char> {
        self.next();
        self.peek_char()
    }

    fn slash(&mut self) -> Result<TokenType> {
        Err(ErrorKind::UnexpectedEof.into())
    }

    fn minus(&mut self) -> TokenType {
        match self.peek_char() {
            Some('-') => self.skip_yield(TokenType::Decrement),
            Some('=') => self.skip_yield(TokenType::MinusEquals),
            _         => TokenType::Minus,
        }
    }

    fn plus(&mut self) -> TokenType {
        match self.peek_char() {
            Some('+') => self.skip_yield(TokenType::Increment),
            Some('=') => self.skip_yield(TokenType::Plus),
            _         => TokenType::Plus,
        }
    }

    fn percent(&mut self) -> TokenType {
        match self.peek_char() {
            Some('=') => self.skip_yield(TokenType::PercentEquals),
            _         => TokenType::Percent
        }
    }

    fn lt(&mut self) -> TokenType {
        match self.peek_char() {
            Some('<') => {
                match self.skip_then_peek() {
                    Some('=') => self.skip_yield(TokenType::LeftShiftEquals),
                    _         => TokenType::LeftShift,
                }
            },
            Some('=') => self.skip_yield(TokenType::LessThanEqualTo),
            _         => TokenType::LessThan,
        }
    }

    fn gt(&mut self) -> TokenType {
        match self.peek_char() {
            Some('=') => self.skip_yield(TokenType::GreaterThanEqualTo),
            Some('>') => {
                match self.skip_then_peek() {
                    Some('>') => {
                        match self.skip_then_peek() {
                            Some('=') => self.skip_yield(TokenType::TripleRightShiftEquals),
                            _         => TokenType::TripleRightShift,
                        }
                    },
                    Some('=') => self.skip_yield(TokenType::RightShiftEquals),
                    _         => TokenType::RightShift,
                }
            },
            _ => TokenType::GreaterThan,
        }
    }

    fn asterisk(&mut self) -> TokenType {
        match self.peek_char() {
            Some('*') => {
                match self.skip_then_peek() {
                    Some('*') => self.skip_yield(TokenType::PowerEquals),
                    _         => TokenType::Power,
                }
            },
            Some('=') => self.skip_yield(TokenType::TimesEquals),
            _         => TokenType::Times,
        }
    }

    fn pipe(&mut self) -> TokenType {
        match self.peek_char() {
            Some('|') => self.skip_yield(TokenType::LogicalOr),
            Some('=') => self.skip_yield(TokenType::BinaryOrEquals),
            _         => TokenType::BinaryOr,
        }
    }

    fn ampersand(&mut self) -> TokenType {
        match self.peek_char() {
            Some('&') => self.skip_yield(TokenType::LogicalAnd),
            Some('=') => self.skip_yield(TokenType::BinaryAndEquals),
            _         => TokenType::BinaryAnd,
        }
    }

    fn bang(&mut self) -> TokenType {
        match self.peek_char() {
            Some('=') => {
                match self.skip_then_peek() {
                    Some('=') => self.skip_yield(TokenType::NotTripleEquals),
                    _         => TokenType::NotEquals,
                }
            },
            _ => TokenType::Bang,
        }
    }

    fn equals(&mut self) -> TokenType {
        match self.peek_char() {
            Some('>') => self.skip_yield(TokenType::Arrow),
            Some('=') => {
                match self.skip_then_peek() {
                    Some('=') => self.skip_yield(TokenType::TripleEquals),
                    _         => TokenType::DoubleEquals,
                }
            },
            _ => TokenType::Equals,
        }
    }

    fn caret(&mut self) -> TokenType {
        match self.peek_char() {
            Some('=') => self.skip_yield(TokenType::BinaryXorEquals),
            _         => TokenType::BinaryXor,
        }
    }

    fn line_terminator_sequence(&mut self, c: char) -> TokenType {
        if c == '\u{000D}' {
            // CR / CRLF
            match self.skip_then_peek() {
                Some('\u{000A}') => self.skip_yield(TokenType::LineTerminator("\u{000D}\u{000A}".to_string())),
                _                => TokenType::LineTerminator('\u{000D}'.to_string()),
            }
        } else {
            TokenType::LineTerminator(c.to_string())
        }
    }
}

impl <'a> Iterator for Tokens<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            let t = self.input.next().map(|x| match x {
                // Punctuators
                (i, '{') => Ok(Token::new(i, TokenType::LeftBrace)),
                (i, '}') => Ok(Token::new(i, TokenType::RightBrace)),
                (i, '(') => Ok(Token::new(i, TokenType::LeftParen)),
                (i, ')') => Ok(Token::new(i, TokenType::RightParen)),
                (i, '[') => Ok(Token::new(i, TokenType::LeftBracket)),
                (i, ']') => Ok(Token::new(i, TokenType::RightBracket)),
                (i, ':') => Ok(Token::new(i, TokenType::Colon)),
                (i, ',') => Ok(Token::new(i, TokenType::Comma)),
                (i, '~') => Ok(Token::new(i, TokenType::Tilde)),
                (i, ';') => Ok(Token::new(i, TokenType::Semicolon)),
                (i, '?') => Ok(Token::new(i, TokenType::Question)),
                (i, '-') => token(i, Ok(self.minus())),
                (i, '+') => token(i, Ok(self.plus())),
                (i, '%') => token(i, Ok(self.percent())),
                (i, '*') => token(i, Ok(self.asterisk())),
                (i, '<') => token(i, Ok(self.lt())),
                (i, '>') => token(i, Ok(self.gt())),
                (i, '|') => token(i, Ok(self.pipe())),
                (i, '&') => token(i, Ok(self.ampersand())),
                (i, '!') => token(i, Ok(self.bang())),
                (i, '=') => token(i, Ok(self.equals())),
                (i, '^') => token(i, Ok(self.caret())),

                // Context sensitive
                (i, '/') => token(i, self.slash()),

                // Newlines
                (i, x) if is_line_terminator(x) => token(i, Ok(self.line_terminator_sequence(x))),

                // Unexpected input
                (i, c)   => Tokens::unexpected_char(i, c),
            });

            match t {
                Some(Ok(x))  => return Some(x),
                Some(Err(e)) => self.diagnostics.push(e),
                None         => return None,
            }
        }
    }
}

fn token(location: usize, maybe: Result<TokenType>) -> Result<Token> {
    maybe.map(|x| Token::new(location, x))
}
