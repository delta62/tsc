use std::fmt;

use super::reservedword::ReservedWord;

#[derive(Debug,PartialEq)]
pub enum TokenType {
    Arrow,
    Bang,
    BinaryAnd,
    BinaryAndEquals,
    BinaryOr,
    BinaryOrEquals,
    BinaryXor,
    BinaryXorEquals,
    Colon,
    Comma,
    Comment(String, CommentStyle),
    Decrement,
    Div,
    DivEqual,
    DoubleEquals,
    Ellipsis,
    Equals,
    GreaterThan,
    GreaterThanEqualTo,
    Identifier(Identifier),
    Increment,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LeftShift,
    LeftShiftEquals,
    LessThan,
    LessThanEqualTo,
    LineTerminator(String),
    LogicalAnd,
    LogicalOr,
    Minus,
    MinusEquals,
    NotEquals,
    NotTripleEquals,
    Number(String),
    Percent,
    PercentEquals,
    Period,
    Plus,
    PlusEquals,
    Power,
    PowerEquals,
    Question,
    RegExp(String, String),
    RightBrace,
    RightBracket,
    RightParen,
    RightShift,
    RightShiftEquals,
    Semicolon,
    String(String, QuoteStyle),
    Template(String),
    Tilde,
    Times,
    TimesEquals,
    TripleEquals,
    TripleRightShift,
    TripleRightShiftEquals,
    WhiteSpace(String),
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenType::Arrow => write!(f, "=>"),
            TokenType::Bang => write!(f, "!"),
            TokenType::BinaryAndEquals => write!(f, "&="),
            TokenType::BinaryAnd => write!(f, "&"),
            TokenType::BinaryOrEquals => write!(f, "|="),
            TokenType::BinaryOr => write!(f, "|"),
            TokenType::BinaryXorEquals => write!(f, "^="),
            TokenType::BinaryXor => write!(f, "^"),
            TokenType::Colon => write!(f, ":"),
            TokenType::Comma => write!(f, ","),
            TokenType::Comment(s, CommentStyle::MultiLine) => write!(f, "/*{}*/", s),
            TokenType::Comment(s, CommentStyle::SingleLine) => write!(f, "//{}", s),
            TokenType::Decrement => write!(f, "--"),
            TokenType::DivEqual => write!(f, "/="),
            TokenType::Div => write!(f, "/"),
            TokenType::DoubleEquals => write!(f, "=="),
            TokenType::Ellipsis => write!(f, "..."),
            TokenType::Equals => write!(f, "="),
            TokenType::GreaterThanEqualTo => write!(f, ">="),
            TokenType::GreaterThan => write!(f, ">"),
            TokenType::Identifier(s) => write!(f, "{}", s),
            TokenType::Increment => write!(f, "++"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::LeftParen => write!(f, "("),
            TokenType::LeftShiftEquals => write!(f, "<<="),
            TokenType::LeftShift => write!(f, "<<"),
            TokenType::LessThanEqualTo => write!(f, "<="),
            TokenType::LessThan => write!(f, "<"),
            TokenType::LineTerminator(s) => write!(f, "{}", s),
            TokenType::LogicalAnd => write!(f, "&&"),
            TokenType::LogicalOr => write!(f, "||"),
            TokenType::MinusEquals => write!(f, "-="),
            TokenType::Minus => write!(f, "-"),
            TokenType::NotEquals => write!(f, "!="),
            TokenType::NotTripleEquals => write!(f, "!=="),
            TokenType::Number(s) => write!(f, "{}", s),
            TokenType::PercentEquals => write!(f, "%="),
            TokenType::Percent => write!(f, "%"),
            TokenType::Period => write!(f, "."),
            TokenType::PlusEquals => write!(f, "+="),
            TokenType::Plus => write!(f, "+"),
            TokenType::PowerEquals => write!(f, "**="),
            TokenType::Power => write!(f, "**"),
            TokenType::Question => write!(f, "?"),
            TokenType::RegExp(body, flags) => write!(f, "/{}/{}", body, flags),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::RightBracket => write!(f, "]"),
            TokenType::RightParen => write!(f, ")"),
            TokenType::RightShiftEquals => write!(f, ">>="),
            TokenType::RightShift => write!(f, ">>"),
            TokenType::Semicolon => write!(f, ";"),
            TokenType::String(s, QuoteStyle::Double) => write!(f, r#""{}""#, s),
            TokenType::String(s, QuoteStyle::Single) => write!(f, "'{}'", s),
            TokenType::Template(s) => write!(f, "{}", s),
            TokenType::Tilde => write!(f, "~"),
            TokenType::TimesEquals => write!(f, "*="),
            TokenType::Times => write!(f, "*"),
            TokenType::TripleEquals => write!(f, "==="),
            TokenType::TripleRightShiftEquals => write!(f, ">>>="),
            TokenType::TripleRightShift => write!(f, ">>>"),
            TokenType::WhiteSpace(s) => write!(f, "{}", s),
        }
    }
}

#[derive(PartialEq,Debug)]
pub enum QuoteStyle {
    Single,
    Double,
}

impl fmt::Display for QuoteStyle {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            QuoteStyle::Single => write!(f, "'"),
            QuoteStyle::Double => write!(f, "\"")
        }
    }
}

#[derive(Debug,PartialEq)]
pub enum Identifier {
    Id(String),
    Reserved(ReservedWord),
}

impl fmt::Display for Identifier {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Identifier::Id(s) => write!(f, "{}", s),
            Identifier::Reserved(rw) => write!(f, "{}", rw),
        }
    }
}

#[derive(Debug,PartialEq)]
pub enum CommentStyle {
    SingleLine,
    MultiLine,
}
