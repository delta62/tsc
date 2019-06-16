use std::fmt;
use super::location::Location;

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

#[derive(Debug)]
pub enum CommentStyle {
    SingleLine,
    MultiLine,
}

#[derive(Debug)]
pub struct Token {
    pub column: u32,
    pub line: u32,
    pub typ: TokenType,
}

#[derive(Debug)]
pub enum ReservedWord {
    // Keyword
    Await,
    Break,
    Case,
    Catch,
    Class,
    Const,
    Continue,
    Debugger,
    Default,
    Delete,
    Do,
    Else,
    Export,
    Extends,
    Finally,
    For,
    Function,
    If,
    Import,
    In,
    InstanceOf,
    New,
    Return,
    Super,
    Switch,
    This,
    Throw,
    Try,
    TypeOf,
    Var,
    Void,
    While,
    With,
    Yield,

    // FutureReservedWord
    Enum,
    Implements,
    Interface,
    Package,
    Private,
    Protected,
    Public,

    // NullLiteral
    Null,
    // BooleanLiteral
    True,
    False,
}

pub fn identifier(text: String) -> TokenType {
    let keyword = match text.as_ref() {
        // Keyword
        "await"      => Some(ReservedWord::Await),
        "break"      => Some(ReservedWord::Break),
        "case"       => Some(ReservedWord::Case),
        "catch"      => Some(ReservedWord::Catch),
        "class"      => Some(ReservedWord::Class),
        "const"      => Some(ReservedWord::Const),
        "continue"   => Some(ReservedWord::Continue),
        "debugger"   => Some(ReservedWord::Debugger),
        "default"    => Some(ReservedWord::Default),
        "delete"     => Some(ReservedWord::Delete),
        "do"         => Some(ReservedWord::Do),
        "else"       => Some(ReservedWord::Else),
        "export"     => Some(ReservedWord::Export),
        "extends"    => Some(ReservedWord::Extends),
        "finally"    => Some(ReservedWord::Finally),
        "for"        => Some(ReservedWord::For),
        "function"   => Some(ReservedWord::Function),
        "if"         => Some(ReservedWord::If),
        "import"     => Some(ReservedWord::Import),
        "in"         => Some(ReservedWord::In),
        "instanceof" => Some(ReservedWord::InstanceOf),
        "new"        => Some(ReservedWord::New),
        "return"     => Some(ReservedWord::Return),
        "super"      => Some(ReservedWord::Super),
        "switch"     => Some(ReservedWord::Switch),
        "this"       => Some(ReservedWord::This),
        "throw"      => Some(ReservedWord::Throw),
        "try"        => Some(ReservedWord::Try),
        "typeof"     => Some(ReservedWord::TypeOf),
        "var"        => Some(ReservedWord::Var),
        "void"       => Some(ReservedWord::Void),
        "while"      => Some(ReservedWord::While),
        "with"       => Some(ReservedWord::With),
        "yield"      => Some(ReservedWord::Yield),

        // FutureReservedWord
        "enum"       => Some(ReservedWord::Enum),
        "implements" => Some(ReservedWord::Implements),
        "interface"  => Some(ReservedWord::Interface),
        "package"    => Some(ReservedWord::Package),
        "private"    => Some(ReservedWord::Private),
        "protected"  => Some(ReservedWord::Protected),
        "public"     => Some(ReservedWord::Public),

        // NullLiteral
        "null"       => Some(ReservedWord::Null),
        // BooleanLiteral
        "true"       => Some(ReservedWord::True),
        "false"      => Some(ReservedWord::False),
        _            => None,
    };

    TokenType::Identifier(text, keyword)
}

#[derive(Debug)]
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
    Identifier(String, Option<ReservedWord>),
    Increment,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LeftShift,
    LeftShiftEquals,
    LessThan,
    LessThanEqualTo,
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
            TokenType::Identifier(s, _) => write!(f, "{}", s),
            TokenType::Increment => write!(f, "++"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::LeftBracket => write!(f, "["),
            TokenType::LeftParen => write!(f, "("),
            TokenType::LeftShiftEquals => write!(f, "<<="),
            TokenType::LeftShift => write!(f, "<<"),
            TokenType::LessThanEqualTo => write!(f, "<="),
            TokenType::LessThan => write!(f, "<"),
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

impl Token {
    pub fn new((line, column): Location, typ: TokenType) -> Token {
        Token { column, line, typ, }
    }
}
