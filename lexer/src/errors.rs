error_chain! {
    errors {
        UnexpectedEof {
            description("unexpected end of file")
            display("Unexpected end of file")
        }

        NotImplemented {
            description("not implemented")
            display("Not implemented")
        }

        UnexpectedChar(c: char, location: usize) {
            description("unexpected character")
            display("Unexpected character '{}' at {}", c, location)
        }

        InvalidCodePoint(val: String, line: u32, column: u32) {
            description("invalid code point")
            display("Invalid code point {} at {}:{}", val, line, column)
        }
    }
}
