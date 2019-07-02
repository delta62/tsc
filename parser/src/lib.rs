#[macro_use]
extern crate error_chain;
extern crate lexer;

mod errors;
mod node;

use errors::*;
use lexer::{Lexer,ReservedWord,Token,TokenType};
use node::Node;

pub struct Parser<I>
where I: Iterator<Item = char> {
    lexer:       Lexer<I>,
    diagnostics: Vec<Error>,
}

impl<I> Parser<I>
where I: Iterator<Item = char> {
    pub fn new(lexer: Lexer<I>) -> Parser<I> {
        Parser {
            lexer:       lexer,
            diagnostics: Vec::new(),
        }
    }

    pub fn diagnostics(&self) -> &Vec<Error> {
        &self.diagnostics
    }

    pub fn script(&mut self) -> Node {
        let statements = self.statement_list();
        let script = Node::Script(statements);
        if let Some(t) = self.lexer.next() {
            self.diagnostics.push(ErrorKind::UnexpectedToken(t.line, t.column).into());
        }
        script
    }

    fn statement_list(&mut self) -> Vec<Node> {
        self.comma_separated_list(|x| {
            match &x {
                &TokenType::Semicolon                 => Ok(Node::EmptyStatement),
                x if rw(x, &ReservedWord::Debugger) => self.debugger_statement(),
                _                                    => Err(ErrorKind::UnexpectedToken(0, 0).into())
            }
        })
    }

    fn debugger_statement(&mut self) -> Result<Node> {
        self.semicolon();
        Ok(Node::DebuggerStatement)
    }

    fn semicolon(&mut self) {

    }

    fn comma_separated_list<P>(&mut self, parser: P) -> Vec<Node>
    where P: FnMut(TokenType) -> Result<Node>,
    {
        let mut items = Vec::new();
        loop {
            match self.lexer.next() {
                Some(t) => {
                    match parser(t.typ) {
                        Ok(n) => items.push(n),
                        Err(e) => self.diagnostics.push(e),
                    }
                    match self.lexer.next() {
                        Some(Token { line: _, column: _, typ: TokenType::Comma }) => (),
                        _ => break,
                    }
                },
                None => break,
            }
        }
        items
    }
}

fn rw(actual: &TokenType, expected: &ReservedWord) -> bool {
    match actual {
        TokenType::Identifier(_, Some(x)) if x == expected => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parses_empty_script() {
        let res = parse("");
        assert_eq!(res.body.len(), 0);
    }

    #[test]
    fn parses_empty_statement() {
        let res = parse(";");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn parses_several_empty_statements() {
        let res = parse(";;;");
        assert_eq!(res.body.len(), 3);
    }

    #[test]
    fn parses_block_statement() {
        let res = parse("{}");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn parses_debug_statement() {
        let res = parse("debugger;");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn inserts_semicolon_at_newline() {
        let res = parse("debugger\n");
        assert_eq!(res.body.len(), 1);
    }

    #[test]
    fn inserts_semicolon_at_eof() {
        verify_single("debugger");
    }

    #[test]
    fn parses_var_stmt() {
        verify_single("var test;");
    }

    #[test]
    fn allows_var_named_yield() {
        verify_single("var yield;");
    }

    #[test]
    fn allows_var_named_await() {
        verify_single("var await;");
    }

    fn parse(input: &str) -> Script {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        parser.script()
    }

    fn verify_single(input: &str) {
        let lexer = Lexer::new(input.chars());
        let mut parser = Parser::new(lexer);
        let res = parser.script();
        assert_eq!(parser.diagnostics.len(), 0, "parse error");
        assert_eq!(res.body.len(), 1, "Expected exactly one node to be parsed");
    }
}
