use std::iter::{Enumerate,Peekable};
use std::str::Chars;

use super::charclass::{is_binary_digit,is_escapable_char,is_id_continue,is_id_start,is_line_terminator,is_octal_digit,is_ws};
use super::errors::*;
use super::token::Token;
use super::tokentype::{CommentStyle,Identifier,QuoteStyle,TokenType};

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

    fn unexpected_eof() -> Result<()> {
        Err(ErrorKind::UnexpectedEof.into())
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

    fn do_times<P, A>(&mut self, times: u32, predicate: P, mut action: A) -> Result<()>
    where P: Fn(char) -> bool, A: FnMut(char) {
        for _ in 0..times {
            match self.input.next() {
                Some((_, c)) if predicate(c) => action(c),
                Some((i, c)) => return Tokens::unexpected_char(i, c).map(|_| ()),
                None         => return Tokens::unexpected_eof(),
            }
        }
        Ok(())
    }

    fn do_required<P, A>(&mut self, predicate: P, mut action: A) -> Result<()>
    where P: Fn(char) -> bool, A: FnMut(char) {
        match self.input.next() {
            Some((_, c)) if predicate(c) => {
                action(c);
                self.do_while(predicate, action);
                Ok(())
            },
            Some((i, c)) => Err(ErrorKind::UnexpectedChar(c, i).into()),
            None         => Err(ErrorKind::UnexpectedEof.into()),
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
        match self.peek_char() {
            Some('/') => self.comment(CommentStyle::SingleLine),
            _         => Err(ErrorKind::NotImplemented.into()),
        }
    }

    fn comment(&mut self, style: CommentStyle) -> Result<TokenType> {
        self.input.next();
        let mut s = "//".to_string();
        self.do_while(|x| !is_line_terminator(x), |x| s.push(x));
        s.shrink_to_fit();
        Ok(TokenType::Comment(s, style))
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
            let next = self.peek_char();
            match next {
                Some('x') | Some('X') => return self.hex_literal(next.unwrap()),
                Some('o') | Some('O') => return self.octal_literal(next.unwrap()),
                Some('b') | Some('B') => return self.binary_literal(next.unwrap()),
                _ => (),
            }
        }

        let mut s = first.to_string();
        self.do_while(|c| c.is_ascii_digit(), |c| s.push(c));
        let mut s = Ok(s);

        if let Some('.') = self.peek_char() {
            s = s.and_then(|mut s| self.decimal().map(|d| s.push_str(&d)).map(|_| s));
        } else if let Some('e') | Some('E') = self.peek_char() {
            let next = self.next_char().unwrap();
            s = s.and_then(|mut s| self.exponent(next).map(|e| s.push_str(&e)).map(|_| s));
        }

        s.map(|mut s| {
            s.shrink_to_fit();
            TokenType::Number(s)
        })
    }

    fn hex_literal(&mut self, next: char) -> Result<TokenType> {
        self.expect(|x| x.is_ascii_hexdigit()).map(|x| {
            let mut s = format!("0{}{}", next, x);
            self.do_while(|x| x.is_ascii_hexdigit(), |x| s.push(x));
            s.shrink_to_fit();
            TokenType::Number(s)
        })
    }

    fn octal_literal(&mut self, next: char) -> Result<TokenType> {
        self.expect(is_octal_digit).map(|x| {
            let mut s = format!("0{}{}", next, x);
            self.do_while(is_octal_digit, |x| s.push(x));
            s.shrink_to_fit();
            TokenType::Number(s)
        })
    }

    fn binary_literal(&mut self, next: char) -> Result<TokenType> {
        self.expect(is_binary_digit).map(|x| {
            let mut s = format!("0{}{}", next, x);
            self.do_while(is_binary_digit, |x| s.push(x));
            s.shrink_to_fit();
            TokenType::Number(s)
        })
    }

    fn decimal(&mut self) -> Result<String> {
        let mut s = '.'.to_string();
        self.expect(|c| c.is_ascii_digit()).map(|c| {
            s.push(c);
            self.do_while(|c| c.is_ascii_digit(), |c| s.push(c));
        })
        .and_then(|_| {
            match self.peek_char() {
                Some('e') | Some('E') => {
                    let next = self.next_char().unwrap();
                    self.exponent(next)
                },
                _                     => Ok("".to_string())
            }
        })
        .map(|e| {
            s.push_str(&e);
            s.shrink_to_fit();
            s
        })
    }

    fn exponent(&mut self, first: char) -> Result<String> {
        let mut s = first.to_string();

        if let Some('+') | Some('-') = self.peek_char() {
            s.push(self.next_char().unwrap());
        }

        self.expect(|c| c.is_ascii_digit()).map(|c| {
            s.push(c);
            self.do_while(|c| c.is_ascii_digit(), |c| s.push(c));
            s.shrink_to_fit();
            s
        })
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
                Some((_, '\\')) => match self.escape_char() {
                    Ok(x)  => s.push_str(&x),
                    Err(e) => return Err(e),
                },
                None => return Err(ErrorKind::UnexpectedEof.into()),
                Some((_, c)) => s.push(c),
            }
        }
        s.shrink_to_fit();
        Ok(TokenType::String(s, quote))
    }

    fn escape_char(&mut self) -> Result<String> {
        self.input.next()
            .ok_or(ErrorKind::UnexpectedEof.into())
            .and_then(|(i, c)| {
                match c {
                    x if is_escapable_char(x) => Ok(format!("\\{}", x)),
                    'u'                       => self.unicode_escape(),
                    'x'                       => self.hex_escape(),
                    _                         => Err(ErrorKind::UnexpectedChar(c, i).into()),
                }
            })
    }

    fn unicode_escape(&mut self) -> Result<String> {
        self.expect(|x| x == '\\')
            .and_then(|_| self.expect(|x| x == 'u'))
            .and_then(|_| {
                let mut s = "\\u".to_string();
                match self.input.next() {
                    Some((_, '{')) => {
                        s.push('{');
                        self.do_required(|x| x.is_ascii_hexdigit(), |x| s.push(x))
                            .and_then(|_| self.expect(|x| x == '}'))
                            .map(|_| {
                                s.push('}');
                                s
                            })
                    },
                    Some((_, c)) if c.is_ascii_hexdigit() => {
                        self.do_times(4, |x| x.is_ascii_hexdigit(), |x| s.push(x)).map(|_| s)
                    },
                    Some((i, c)) => Err(ErrorKind::UnexpectedChar(c, i).into()),
                    None         => Err(ErrorKind::UnexpectedEof.into()),
                }
            })
            .map(|mut s| {
                s.shrink_to_fit();
                s
            })
    }

    fn hex_escape(&mut self) -> Result<String> {
        let mut s = "\\x".to_string();
        self.do_times(2, |x| x.is_ascii_hexdigit(), |x| s.push(x))
            .map(|_| {
                s.shrink_to_fit();
                s
            })
    }

    fn identifier(&mut self, first: char) -> Result<TokenType> {
        match first {
            '\\' => self.unicode_escape(),
            c    => Ok(c.to_string()),
        }.and_then(|mut s| {
            while self.peek_char().map_or(false, is_id_continue) {
                let next = self.next_char().unwrap();
                match next {
                    // <ZWNJ> <ZWJ>
                    '\u{200C}' => match self.expect(|x| x == '\u{200D}') {
                        Ok(_) => s.push_str("\u{200C}\u{200D}"),
                        Err(e) => return Err(e)
                    },
                    '\\' => match self.unicode_escape() {
                        Ok(e) => s.push_str(&e),
                        Err(e) => return Err(e),
                    },
                    c => s.push(c)
                }
            }
            s.shrink_to_fit();
            Ok(TokenType::Identifier(Identifier::Id(s)))
        })
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
                (i, x) if is_id_start(x) => token(i, self.identifier(x)),

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
