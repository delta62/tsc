#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;
mod node;

use errors::*;
use lexer::{Identifier,LanguageWord,Lexer,Token,TokenType,Tokens};
use node::Node;

pub struct Parser<'a> {
    tokens:      Tokens<'a>,
    diagnostics: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: Lexer<'a>) -> Parser<'a> {
        Parser {
            tokens:      lexer.into_iter(),
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &Vec<Error> {
        &self.diagnostics
    }

    fn unexpected_token(&mut self, token: Token) {
        self.diagnostics.push(ErrorKind::UnexpectedToken(token.location).into());
    }

    fn unexpected_eof(&mut self) {
        self.diagnostics.push(ErrorKind::UnexpectedEof.into());
    }

    pub fn script(&mut self) -> Node {
        let statements = self.statement_list();
        let script = Node::Script(statements);
        if let Some(t) = self.tokens.next() {
            self.unexpected_token(t);
        }
        script
    }

    fn statement_list(&mut self) -> Vec<Node> {
        let mut items = Vec::new();
        loop {
            let next = self.tokens
                .next()
                .map(|t| t.typ)
                .and_then(|t| match t {
                    TokenType::Identifier(Identifier::Special(LanguageWord::Const)) |
                    TokenType::Identifier(Identifier::Special(LanguageWord::Let)) => {
                        Some(self.lexical_declaration())
                    },
                    _ => {
                        None
                    },
                });
            match next {
                Some(t) => items.push(t),
                None => break,
            }
        }
        items
    }

    fn lexical_declaration(&mut self) -> Node {
        // let or const
        // let prefix = self.tokens.next().unwrap();

        // binding list
        // lexical binding
        match self.tokens.next().map(|t| t.typ) {
            Some(TokenType::Identifier(Identifier::Id(s))) => {
                Node::BindingIdentifier(s)
            },
            Some(_) => panic!("not implemented"),
            None => panic!("Unexpected EOF"),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_empty_script() {
        parse("");
    }

    #[test]
    fn parses_let_declaration() {
        parse("let a");
    }

    fn parse(input: &str) -> Node {
        let lexer = Lexer::with_str(input);
        let mut parser = Parser::new(lexer);
        let ret = parser.script();
        assert_eq!(parser.diagnostics().len(), 0, "Parse errors were encountered");
        ret
    }
}
