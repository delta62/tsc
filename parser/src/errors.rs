error_chain! {

    links {
        Lex(lexer::errors::Error, lexer::errors::ErrorKind);
    }

    errors {
        UnexpectedEof

        NotImplemented

        UnexpectedToken(line: u32, col: u32) {
            description("unexpected token")
            display("Unexpected token at {}:{}", line, col)
        }
    }
}
