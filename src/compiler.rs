use crate::{
    chunk::{Chunk, OpCode, Value},
    scanner::{Token, TokenType},
};

pub fn compile(tokens: Vec<Token>) -> Result<Chunk, &'static str> {
    let mut parser = Parser::new(tokens);
    parser.advance();
    parser.expression();
    parser.consume(TokenType::Eof, "Expected end of expression");
    parser.end_compiler();
    if parser.had_error {
        // TODO: consider returning an actual error here
        return Err("");
    }
    Ok(parser.chunk)
}

struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    had_error: bool,
    panic_mode: bool,
    tokens: Vec<Token>,   // Token list with scanned tokens
    current_index: usize, // Token index
    chunk: Chunk,
}

#[derive(PartialEq, PartialOrd)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or
    And,        // and
    Equality,   // == !=
    Comparison, // < > <= >=
    Term,       // + -
    Factor,     // * /
    Unary,      // ! -
    Call,       // . ()
    Primary,
}

impl Precedence {
    // Returns the next precedence level
    // In the book, this is achieved by simply incrementing the precedence by 1
    fn next(&self) -> Precedence {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Primary,
            Precedence::Primary => unreachable!(),
        }
    }
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self {
            current: None,
            previous: None,
            had_error: false,
            panic_mode: false,
            chunk: Chunk::new(),
            tokens,
            current_index: 0,
        }
    }

    fn advance(&mut self) {
        self.previous = self.current.clone();
        loop {
            if self.current_index >= self.tokens.len() {
                break;
            }
            let token = self.tokens[self.current_index].clone();
            self.current = Some(token.clone());
            self.current_index += 1;
            if token.kind != TokenType::Error {
                break;
            }
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

    fn emit_byte(&mut self, byte: OpCode) {
        let line = self.previous.clone().unwrap().line;
        self.chunk.write(byte, line);
    }

    fn end_compiler(&mut self) {
        self.emit_byte(OpCode::Return);
    }

    fn binary(&mut self) {
        // At this point we already know we are parsing a binary operator
        // i.e. we already consumed the first operand and the operator,
        // now we need to parse the rest of the expression
        let operator_type = self.previous.clone().unwrap().kind;
        let rule = Self::get_rule(operator_type);
        self.parse_precedence(rule.precedence.next());

        match operator_type {
            TokenType::Plus => {
                self.emit_byte(OpCode::Add);
            }
            TokenType::Minus => {
                self.emit_byte(OpCode::Subtract);
            }
            TokenType::Star => {
                self.emit_byte(OpCode::Multiply);
            }
            TokenType::Slash => {
                self.emit_byte(OpCode::Divide);
            }
            _ => unreachable!(),
        }
    }

    fn expression(&mut self) {
        self.parse_precedence(Precedence::Assignment);
    }

    fn grouping(&mut self) {
        // Compile the expression wrapped in parentheses
        self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
    }

    fn number(&mut self) {
        // The token has already been consumed and is stored in self.previous
        let value_lexeme = self.previous.clone().unwrap().lexeme;
        let value = Value::Number(value_lexeme.parse::<f64>().unwrap());
        self.emit_constant(value);
    }

    fn unary(&mut self) {
        let operator_type = self.previous.clone().unwrap().kind;
        // Compile the operand
        self.parse_precedence(Precedence::Unary);

        match operator_type {
            TokenType::Minus => {
                self.emit_byte(OpCode::Negate);
            }
            _ => unreachable!(),
        }
    }

    // Parses a prefix expression, and attempts to parse an infix expression with the next tokens,
    // as long as their precedence is higher than the current.
    fn parse_precedence(&mut self, precedence: Precedence) {
        self.advance();
        let previous_token_type = self.previous.clone().unwrap().kind;
        // The first token is always going to belong to a prefix expression, by definition
        let Some(prefix_rule) = Self::get_rule(previous_token_type).prefix else {
            self.error("Expect expression.");
            return;
        };
        // Consume the prefix expression tokens
        prefix_rule(self);

        // Attempt to parse an infix expression
        while precedence <= Self::get_rule(self.current.clone().unwrap().kind).precedence {
            // Consume the infix expression token (e.g. the right operand)
            self.advance();
            let infix_rule = Self::get_rule(self.previous.clone().unwrap().kind)
                .infix
                .unwrap();
            infix_rule(self);
        }
    }

    fn emit_constant(&mut self, value: Value) {
        let constant_index = self.make_constant(value);
        self.emit_byte(OpCode::Constant(constant_index));
    }

    /// Adds the `value` constant to the chunk and returns its index
    fn make_constant(&mut self, value: Value) -> usize {
        let constant_index = self.chunk.add_constant(value);
        constant_index
    }

    fn error_at_current(&mut self, message: &str) {
        let token = &self.current.clone().unwrap();
        self.error_at(token, message);
    }

    fn error(&mut self, message: &str) {
        let token = self.previous.clone().unwrap();
        self.error_at(&token, message);
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

    /// Given a token type, returns a `ParseRule` struct, which contains:
    ///  * the function to compile a prefix expression starting with a token of that type
    ///  * the function to compile an infix expression whose left operand is followed by a token of that type
    ///  * the precedence of an infix expression that uses that token as an operator
    fn get_rule(token_type: TokenType) -> ParseRule {
        match token_type {
            TokenType::LeftParen => ParseRule::new(Some(Parser::grouping), None, Precedence::None),
            TokenType::Minus => {
                ParseRule::new(Some(Parser::unary), Some(Parser::binary), Precedence::Term)
            }
            TokenType::Plus => ParseRule::new(None, Some(Parser::binary), Precedence::Term),
            TokenType::Slash => ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Star => ParseRule::new(None, Some(Parser::binary), Precedence::Factor),
            TokenType::Number => ParseRule::new(Some(Parser::number), None, Precedence::None),
            _ => ParseRule::default(),
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Parser)>,
    infix: Option<fn(&mut Parser)>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser)>,
        infix: Option<fn(&mut Parser)>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }

    fn default() -> Self {
        Self {
            prefix: None,
            infix: None,
            precedence: Precedence::None,
        }
    }
}
