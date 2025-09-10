use crate::{
    chunk::Chunk,
    scanner::{Scanner, Token, TokenType},
};

pub fn compile(source: String) -> Result<(), &'static str> {
    let mut parser = Parser::new(source);
    parser.advance();
    // expression(); TODO
    parser.consume(TokenType::Eof, "Expected end of expression");
    if parser.had_error {
        return Err("");
    }
    Ok(())
}

struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    // TODO: consider removing scanner from here
    scanner: Scanner,
}

impl Parser {
    fn new(source: String) -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
            scanner: Scanner::new(source),
        }
    }

    fn advance(&mut self) {
        loop {
            let token = self.scanner.scan_token();
            if token.kind != TokenType::Error {
                break;
            }
            self.current = Some(token);
            self.error_at_current("");
        }
    }

    fn consume(&mut self, token_type: TokenType, message: &str) {
        if let Some(token) = &self.current {
            if token.kind == token_type {
                self.advance();
                return;
            }
        }
        self.error_at_current(message);
    }

    fn error_at_current(&mut self, message: &str) {
        let token = &self.current.clone().unwrap();
        self.error_at(token, message);
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => {
                eprint!(" at end");
            }
            // Do nothing
            TokenType::Error => (),
            _ => {
                // TODO: check if it is correct to print the lexeme here
                // https://craftinginterpreters.com/compiling-expressions.html#handling-syntax-errors
                eprint!(" at '{}'", token.lexeme);
            }
        }
        eprintln!(": {message}");
        self.had_error = true;
    }
}
