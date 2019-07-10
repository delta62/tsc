use std::iter::{Enumerate,Peekable};
use std::str::Chars;

use super::charclass::{is_binary_digit,is_id_continue,is_id_start,is_line_terminator,is_octal_digit,is_ws};
use super::errors::*;
use super::token::Token;
use super::tokentype::{Identifier,QuoteStyle,TokenType};

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

    fn skip_yield(&mut self, result: TokenType) -> TokenType {
        self.input.next();
        result
    }

    fn expect<P>(&mut self, predicate: P) -> Result<char>
    where P: FnOnce(char) -> bool {
        self.input.next()
            .ok_or(ErrorKind::UnexpectedEof.into())
            .and_then(|(i, x)| {
                if predicate(x) {
                    Ok(x)
                } else {
                    Err(ErrorKind::UnexpectedChar(x, i).into())
                }
            })
    }

    fn do_while<P, A>(&mut self, predicate: P, mut action: A)
    where P: Fn(char) -> bool, A: FnMut(char) {
        while self.peek_char().map_or(false, |x| predicate(x)) {
            action(self.next_char().unwrap());
        }
    }

    fn peek_char(&mut self) -> Option<char> {
        match self.input.peek() {
            Some((_, c)) => Some(*c),
            None         => None,
        }
    }

    fn next_char(&mut self) -> Option<char> {
        match self.input.next() {
            Some((_, c)) => Some(c),
            None         => None,
        }
    }

    fn skip_then_peek(&mut self) -> Option<char> {
        self.next();
        self.peek_char()
    }

    fn slash(&mut self) -> Result<TokenType> {
        Err(ErrorKind::NotImplemented.into())
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

    fn period(&mut self) -> Result<TokenType> {
        match self.peek_char() {
            Some('.') => {
                self.input.next();
                match self.input.next() {
                    Some((_, '.')) => Ok(TokenType::Ellipsis),
                    Some((i, c))   => Err(ErrorKind::UnexpectedChar(c, i).into()),
                    None           => Err(ErrorKind::UnexpectedEof.into()),
                }
            },
            Some(c) if c.is_ascii_hexdigit() => self.decimal().map(|x| TokenType::Number(x)),
            _ => Ok(TokenType::Period)
        }
    }

    fn digit(&mut self, first: char) -> Result<TokenType> {
        if let '0' = first {
            match self.peek_char() {
                Some('x') | Some('X') => return self.hex_literal(),
                Some('o') | Some('O') => return self.octal_literal(),
                Some('b') | Some('B') => return self.binary_literal(),
                _         => (),
            }
        }

        let mut s = first.to_string();
        self.do_while(|c| c.is_ascii_digit(), |c| s.push(c));

        let mut s = Ok(s);

        if let Some('.') = self.peek_char() {
            s = s.and_then(|mut s| self.decimal().map(|d| s.push_str(&d)).map(|_| s));
        }

        if let Some('e') | Some('E') = self.peek_char() {
            s = s.and_then(|mut s| self.exponent().map(|e| s.push_str(&e)).map(|_| s));
        }

        s.map(|mut s| {
            s.shrink_to_fit();
            s
        })
        .map(|s| TokenType::Number(s))
    }

    fn hex_literal(&mut self) -> Result<TokenType> {
        self.expect(|x| x.is_ascii_hexdigit())
            .map(|x| {
                let mut s = format!("0{}", x);
                self.do_while(|x| x.is_ascii_hexdigit(), |x| s.push(x));
                s.shrink_to_fit();
                TokenType::Number(s)
            })
    }

    fn octal_literal(&mut self) -> Result<TokenType> {
        self.expect(is_octal_digit)
            .map(|x| {
                let mut s = format!("0{}", x);
                self.do_while(is_octal_digit, |x| s.push(x));
                s.shrink_to_fit();
                TokenType::Number(s)
            })
    }

    fn binary_literal(&mut self) -> Result<TokenType> {
        self.expect(is_binary_digit)
            .map(|x| {
                let mut s = format!("0{}", x);
                self.do_while(is_binary_digit, |x| s.push(x));
                s.shrink_to_fit();
                TokenType::Number(s)
            })
    }

    fn decimal(&mut self) -> Result<String> {
        let mut s = '.'.to_string();
        match self.input.next() {
            Some((_, c)) if c.is_ascii_digit() => {
                s.push(c);
                while self.peek_char().map_or(false, |c| c.is_ascii_digit()) {
                    s.push(self.next_char().unwrap());
                }
                s.shrink_to_fit();
                Ok(s)
            },
            Some((i, c)) => Err(ErrorKind::UnexpectedChar(c, i).into()),
            None         => Err(ErrorKind::UnexpectedEof.into()),
        }
    }

    fn exponent(&mut self) -> Result<String> {
        Err(ErrorKind::UnexpectedEof.into())
    }

    fn template(&mut self) -> Result<TokenType> {
        let mut s = String::new();
        loop {
            match self.next_char() {
                Some('$') => {
                    s.push('$');
                    match self.peek_char() {
                        Some(c) => {
                            s.push(c);
                            if c == '{' {
                                break
                            }
                        },
                        None => return Err(ErrorKind::UnexpectedEof.into())
                    }
                },
                Some('\\') => {
                    s.push('\\');
                    match self.next_char() {
                        Some(c) => s.push(c),
                        None    => return Err(ErrorKind::UnexpectedEof.into())
                    }
                },
                Some('`') => {
                    s.push('`');
                    break
                },
                Some(c) => s.push(c),
                None => return Err(ErrorKind::UnexpectedEof.into()),
            }
        }
        s.shrink_to_fit();
        Ok(TokenType::Template(s))
    }

    fn string(&mut self, quote: QuoteStyle) -> Result<TokenType> {
        let mut s = String::new();
        loop {
            match self.input.next() {
                Some((_, '\'')) if quote == QuoteStyle::Single => break,
                Some((_, '"'))  if quote == QuoteStyle::Double => break,
                Some((i, c))    if is_line_terminator(c)       => return Err(ErrorKind::UnexpectedChar(c, i).into()),
                Some((_, '\\'))                                => return Err(ErrorKind::NotImplemented.into()),
                None                                           => return Err(ErrorKind::UnexpectedEof.into()),
                Some((_, c))                                   => s.push(c),
            }
        }
        s.shrink_to_fit();
        Ok(TokenType::String(s, quote))
    }

    // TODO escape sequences
    fn identifier(&mut self, first: char) -> TokenType {
        let mut s = first.to_string();
        while self.peek_char().map_or(false, is_id_continue) {
            s.push(self.next_char().unwrap());
        }
        s.shrink_to_fit();
        TokenType::Identifier(Identifier::Id(s))
    }

    fn whitespace(&mut self, first: char) -> TokenType {
        let mut s = first.to_string();
        while self.peek_char().map_or(false, is_ws) {
            s.push(self.next_char().unwrap());
        }
        s.shrink_to_fit();
        TokenType::WhiteSpace(s)
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
                (i, '.') => token(i, self.period()),

                // Literals
                (i, d) if d.is_ascii_digit() => token(i, self.digit(d)),
                (i, '`')  => token(i, self.template()),
                (i, '\'') => token(i, self.string(QuoteStyle::Single)),
                (i, '"')  => token(i, self.string(QuoteStyle::Double)),

                // Identifiers
                (i, x) if is_id_start(x) => token(i, Ok(self.identifier(x))),

                // Whitespace
                (i, x) if is_ws(x) => token(i, Ok(self.whitespace(x))),

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
