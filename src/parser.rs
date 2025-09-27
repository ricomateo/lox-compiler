use crate::{
    declaration::{Declaration, Statement},
    expr::{Expr, Literal},
    scanner::{Token, TokenType},
};

#[derive(Debug, PartialEq, PartialOrd)]
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

pub struct Parser {
    current: Option<Token>,
    previous: Option<Token>,
    tokens: Vec<Token>,
    current_index: usize,
    had_error: bool,
    panic_mode: bool,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let mut current = None;
        let mut idx = 0;
        if !tokens.is_empty() {
            current = Some(tokens[0].clone());
            idx = 1;
        }
        Self {
            current,
            previous: None,
            tokens,
            current_index: idx,
            had_error: false,
            panic_mode: false,
        }
    }

    pub fn parse(&mut self) -> Vec<Declaration> {
        let mut declarations = Vec::new();
        while !self.matches(TokenType::Eof) {
            let declaration = self.declaration();
            //  else {
            //     // Jump to the next statement in case of error
            //     // self.synchronize();
            //     continue;
            // };
            declarations.push(declaration);
        }
        declarations
    }

    fn declaration(&mut self) -> Declaration {
        self.statement()
    }

    fn statement(&mut self) -> Declaration {
        if self.matches(TokenType::Print) {
            return self.print_statement();
        } else {
            return self.expression_statement();
        }
    }

    fn expression_statement(&mut self) -> Declaration {
        let expr = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after expression.");
        Declaration::Statement(Statement::ExprStatement(expr))
    }

    fn matches(&mut self, token_type: TokenType) -> bool {
        if !self.check(token_type) {
            return false;
        }
        self.advance();
        true
    }

    fn check(&self, token_type: TokenType) -> bool {
        self.current.clone().unwrap().kind == token_type
    }

    fn print_statement(&mut self) -> Declaration {
        let expr = self.expression();
        self.consume(TokenType::Semicolon, "Expect ';' after value.");
        Declaration::Statement(Statement::PrintStatement(expr))
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Expr {
        self.advance();

        if self.previous.is_none() {
            self.previous = self.current.clone();
        }

        let previous_token_type = self.previous.clone().unwrap().kind;
        let prefix_rule = get_rule(previous_token_type).prefix;
        if prefix_rule.is_none() {
            self.error("Expect expression.");
            return Expr::Literal(Literal::Number(0.0));
        }

        let mut expr = prefix_rule.unwrap()(self);

        while precedence <= get_rule(self.current.clone().unwrap().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.clone().unwrap().kind).infix;
            if let Some(rule) = infix_rule {
                expr = rule(self, expr);
            }
        }

        expr
    }

    // ---------- Parsing rules ----------

    pub fn expression(&mut self) -> Expr {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self) -> Expr {
        let value = self
            .previous
            .clone()
            .unwrap()
            .lexeme
            .parse::<f64>()
            .unwrap();
        Expr::Literal(Literal::Number(value))
    }

    fn string(&mut self) -> Expr {
        let length = self.previous.clone().unwrap().length;
        // Remove quotes from string
        let string = &self.previous.clone().unwrap().lexeme[1..length - 1];
        Expr::Literal(Literal::String(string.into()))
    }

    fn unary(&mut self) -> Expr {
        let operator = self.previous.clone().unwrap();
        let right = self.parse_precedence(Precedence::Unary);
        Expr::Unary {
            operator,
            right: Box::new(right),
        }
    }

    fn binary(&mut self, left: Expr) -> Expr {
        let operator = self.previous.clone().unwrap();
        let rule = get_rule(operator.kind);
        let right = self.parse_precedence(rule.precedence.next());
        Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }

    fn grouping(&mut self) -> Expr {
        let expr = self.expression();
        self.consume(TokenType::RightParen, "Expect ')' after expression.");
        Expr::Grouping {
            expression: Box::new(expr),
        }
    }

    fn literal(&mut self) -> Expr {
        match &self.previous.clone().unwrap().kind {
            TokenType::False => Expr::Literal(Literal::Bool(false)),
            TokenType::True => Expr::Literal(Literal::Bool(true)),
            TokenType::Nil => Expr::Literal(Literal::Nil),
            _ => unreachable!(),
        }
    }

    // ---------- Helpers ----------

    fn advance(&mut self) {
        if let Some(current_token) = &self.current {
            self.previous = Some(current_token.clone());
        }

        loop {
            if self.current_index >= self.tokens.len() {
                self.current = Some(Token {
                    kind: TokenType::Eof,
                    start: 0,
                    length: 0,
                    line: 0,
                    lexeme: String::new(),
                });
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
        self.error(message);
    }

    // ---------- Error handling ----------

    fn error(&mut self, message: &str) {
        if let Some(token) = &self.previous {
            let token = token.clone();
            self.error_at(&token, message);
        }
    }

    fn error_at(&mut self, token: &Token, _message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}'", token.lexeme),
        }

        self.had_error = true;
    }

    fn error_at_current(&mut self, message: &str) {
        if let Some(token) = &self.current {
            let token = token.clone();
            self.error_at(&token, message);
        } else {
            self.error(message);
        }
    }
}

struct ParseRule {
    prefix: Option<fn(&mut Parser) -> Expr>,
    infix: Option<fn(&mut Parser, Expr) -> Expr>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser) -> Expr>,
        infix: Option<fn(&mut Parser, Expr) -> Expr>,
        precedence: Precedence,
    ) -> Self {
        Self {
            prefix,
            infix,
            precedence,
        }
    }
}

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
        TokenType::False => ParseRule::new(Some(Parser::literal), None, Precedence::None),
        TokenType::True => ParseRule::new(Some(Parser::literal), None, Precedence::None),
        TokenType::Nil => ParseRule::new(Some(Parser::literal), None, Precedence::None),
        TokenType::Bang => ParseRule::new(Some(Parser::unary), None, Precedence::None),
        TokenType::EqualEqual => ParseRule::new(None, Some(Parser::binary), Precedence::Equality),
        TokenType::BangEqual => ParseRule::new(None, Some(Parser::binary), Precedence::Equality),
        TokenType::Greater => ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
        TokenType::GreaterEqual => {
            ParseRule::new(None, Some(Parser::binary), Precedence::Comparison)
        }
        TokenType::Less => ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
        TokenType::LessEqual => ParseRule::new(None, Some(Parser::binary), Precedence::Comparison),
        TokenType::String => ParseRule::new(Some(Parser::string), None, Precedence::None),
        _ => ParseRule::new(None, None, Precedence::None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::expr::Expr;
    use crate::scanner::Scanner;

    fn scan_and_parse(source: &str) -> Expr {
        let mut scanner = Scanner::new(source.into());
        let tokens = scanner.scan();
        let mut parser = Parser::new(tokens);
        parser.expression()
    }

    #[test]
    fn test_literal() {
        let source = "1";
        let ast = scan_and_parse(source);
        assert_eq!(ast, Expr::Literal(Literal::Number(1.0)))
    }

    #[test]
    fn test_string() {
        let source = "\"hello\"";
        let ast = scan_and_parse(source);
        assert_eq!(ast, Expr::Literal(Literal::String("hello".into())))
    }

    #[test]
    fn test_unary() {
        let source = "-1";
        let ast = scan_and_parse(source);
        let Expr::Unary { operator, right } = ast else {
            panic!("Expected unary expression");
        };
        let Expr::Literal(literal) = *right else {
            panic!("Expected literal");
        };
        assert_eq!(operator.kind, TokenType::Minus);
        assert_eq!(literal, Literal::Number(1.0));
    }

    #[test]
    fn test_grouping() {
        let source = "(1)";
        let ast = scan_and_parse(source);
        let Expr::Grouping { expression } = ast else {
            panic!("Expected grouping expression");
        };
        let Expr::Literal(literal) = *expression else {
            panic!("Expected literal");
        };
        assert_eq!(literal, Literal::Number(1.0));
    }

    #[test]
    fn test_grouping_and_unary() {
        let source = "(-1)";
        let ast = scan_and_parse(source);
        let Expr::Grouping { expression } = ast else {
            panic!("Expected grouping expression");
        };
        let Expr::Unary { operator, right } = *expression else {
            panic!("Expected unary expression");
        };
        let Expr::Literal(literal) = *right else {
            panic!("Expected literal");
        };
        assert_eq!(literal, Literal::Number(1.0));
        assert_eq!(operator.kind, TokenType::Minus);
    }

    #[test]
    fn test_binary() {
        let source = "2 + 3";
        let ast = scan_and_parse(source);
        let Expr::Binary {
            left,
            operator,
            right,
        } = ast
        else {
            panic!("Expected binary expression");
        };
        let Expr::Literal(left) = *left else {
            panic!("Expected literal at left");
        };
        let Expr::Literal(right) = *right else {
            panic!("Expected literal at right");
        };
        assert_eq!(left, Literal::Number(2.0));
        assert_eq!(operator.kind, TokenType::Plus);
        assert_eq!(right, Literal::Number(3.0));
    }

    #[test]
    fn test_more_complex_binary() {
        let source = "(2 - -3) / (3 * 4)";
        let ast = scan_and_parse(source);
        let Expr::Binary {
            left,
            operator,
            right,
        } = ast
        else {
            panic!("Expected binary expression");
        };

        // Check the left operand (2 - -3)
        {
            let Expr::Grouping { expression } = *left else {
                panic!("Expected a grouping expression as the first operand");
            };
            let Expr::Binary {
                left,
                operator,
                right,
            } = *expression
            else {
                panic!("Expected binary as the first grouping expression");
            };
            assert_eq!(*left, Expr::Literal(Literal::Number(2.0)));
            assert_eq!(operator.kind, TokenType::Minus);
            // Check the right operand (-3)
            {
                let Expr::Unary { operator, right } = *right else {
                    panic!("Expected unary");
                };
                assert_eq!(operator.kind, TokenType::Minus);
                assert_eq!(*right, Expr::Literal(Literal::Number(3.0)));
            }
        }

        // Check the operator
        assert_eq!(operator.kind, TokenType::Slash);

        // Check the right operand (3 * 4)
        {
            let Expr::Grouping { expression } = *right else {
                panic!("Expected a grouping expression as the second operand");
            };
            let Expr::Binary {
                left,
                operator,
                right,
            } = *expression
            else {
                panic!("Expected binary as the second grouping expression");
            };
            assert_eq!(*left, Expr::Literal(Literal::Number(3.0)));
            assert_eq!(operator.kind, TokenType::Star);
            assert_eq!(*right, Expr::Literal(Literal::Number(4.0)));
        }
    }
}
