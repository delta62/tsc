#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;
mod node;

use errors::*;
use lexer::{Lexer,Token,Tokens};
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

    pub fn script(&mut self) -> Node {
        let statements = self.statement_list();
        let script = Node::Script(statements);
        if let Some(t) = self.tokens.next() {
            self.unexpected_token(t);
        }
        script
    }

    fn statement_list(&mut self) -> Vec<Node> {
        Vec::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_empty_script() {
        parse("");
    }

    fn parse(input: &str) -> Node {
        let lexer = Lexer::with_str(input);
        let mut parser = Parser::new(lexer);
        let ret = parser.script();
        assert_eq!(parser.diagnostics().len(), 0);
        ret
    }
}
