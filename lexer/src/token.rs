use super::tokentype::TokenType;

#[derive(Debug)]
pub struct Token {
    pub location: usize,
    pub typ: TokenType,
}

impl Token {
    pub fn new(location: usize, typ: TokenType) -> Token {
        Token { location, typ }
    }
}
