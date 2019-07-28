error_chain! {

    links {
        Lex(lexer::errors::Error, lexer::errors::ErrorKind);
    }

    errors {
        UnexpectedEof

        NotImplemented

        UnexpectedToken(location: usize) {
            description("unexpected token")
            display("Unexpected token at {}", location)
        }
    }
}
