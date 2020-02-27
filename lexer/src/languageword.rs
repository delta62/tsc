use std::fmt;

#[derive(Copy,Clone,Debug,PartialEq)]
pub enum LanguageWord {
    // Keyword
    Async,
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
    Let,
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

impl fmt::Display for LanguageWord {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            LanguageWord::Async => write!(f, "async"),
            LanguageWord::Await => write!(f, "await"),
            LanguageWord::Break => write!(f, "break"),
            LanguageWord::Case => write!(f, "case"),
            LanguageWord::Catch => write!(f, "catch"),
            LanguageWord::Class => write!(f, "class"),
            LanguageWord::Const => write!(f, "const"),
            LanguageWord::Continue => write!(f, "continue"),
            LanguageWord::Debugger => write!(f, "debugger"),
            LanguageWord::Default => write!(f, "default"),
            LanguageWord::Delete => write!(f, "delete"),
            LanguageWord::Do => write!(f, "do"),
            LanguageWord::Else => write!(f, "else"),
            LanguageWord::Export => write!(f, "export"),
            LanguageWord::Extends => write!(f, "extends"),
            LanguageWord::Finally => write!(f, "finally"),
            LanguageWord::For => write!(f, "for"),
            LanguageWord::Function => write!(f, "function"),
            LanguageWord::If => write!(f, "if"),
            LanguageWord::Import => write!(f, "import"),
            LanguageWord::In => write!(f, "in"),
            LanguageWord::InstanceOf => write!(f, "instanceof"),
            LanguageWord::Let => write!(f, "let"),
            LanguageWord::New => write!(f, "new"),
            LanguageWord::Return => write!(f, "return"),
            LanguageWord::Super => write!(f, "super"),
            LanguageWord::Switch => write!(f, "switch"),
            LanguageWord::This => write!(f, "this"),
            LanguageWord::Throw => write!(f, "throw"),
            LanguageWord::Try => write!(f, "try"),
            LanguageWord::TypeOf => write!(f, "typeof"),
            LanguageWord::Var => write!(f, "var"),
            LanguageWord::Void => write!(f, "void"),
            LanguageWord::While => write!(f, "while"),
            LanguageWord::With => write!(f, "with"),
            LanguageWord::Yield => write!(f, "yield"),
            LanguageWord::Enum => write!(f, "enum"),
            LanguageWord::Implements => write!(f, "implements"),
            LanguageWord::Interface => write!(f, "interface"),
            LanguageWord::Package => write!(f, "package"),
            LanguageWord::Private => write!(f, "private"),
            LanguageWord::Protected => write!(f, "protected"),
            LanguageWord::Public => write!(f, "public"),
            LanguageWord::Null => write!(f, "null"),
            LanguageWord::True => write!(f, "true"),
            LanguageWord::False => write!(f, "false"),
        }
    }
}
