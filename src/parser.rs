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
    pub had_error: bool,
    panic_mode: bool,
}

#[derive(Debug)]
pub enum ParseError {
    ExpectedExpression,
    ExpectedNumber,
    ExpectedToken(TokenType, String),
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
            let Ok(declaration) = self.declaration() else {
                self.synchronize();
                continue;
            };
            declarations.push(declaration);
        }
        declarations
    }

    fn declaration(&mut self) -> Result<Declaration, ParseError> {
        if self.matches(TokenType::Var) {
            return self.var_declaration();
        }
        self.statement()
    }

    fn statement(&mut self) -> Result<Declaration, ParseError> {
        if self.matches(TokenType::Print) {
            return self.print_statement();
        } else if self.matches(TokenType::For) {
            return self.for_statement();
        } else if self.matches(TokenType::LeftBrace) {
            return self.block();
        } else if self.matches(TokenType::If) {
            self.if_statement()
        } else if self.matches(TokenType::While) {
            self.while_statement()
        } else {
            return self.expression_statement();
        }
    }

    fn while_statement(&mut self) -> Result<Declaration, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after condition.")?;

        let body = Box::new(self.statement()?);

        let line = self.previous_token_line();
        let declaration = Declaration::while_statement(condition, body, line);
        Ok(declaration)
    }

    fn for_statement(&mut self) -> Result<Declaration, ParseError> {
        let line = self.previous_token_line();
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        let initializer_clause = self.parse_initializer_clause()?;
        let condition_clause = self.parse_condition_clause()?;
        let increment_clause = self.parse_increment_clause()?;

        let body = self.statement()?;
        let for_statement = Declaration::for_statement(
            initializer_clause,
            condition_clause,
            increment_clause,
            body,
            line,
        );
        Ok(for_statement)
    }

    fn parse_initializer_clause(&mut self) -> Result<Option<Declaration>, ParseError> {
        let initializer_clause = if self.matches(TokenType::Semicolon) {
            None
        } else if self.matches(TokenType::Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };
        Ok(initializer_clause)
    }

    fn parse_condition_clause(&mut self) -> Result<Option<Expr>, ParseError> {
        if !self.matches(TokenType::Semicolon) {
            let condition = self.expression()?;
            self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
            return Ok(Some(condition));
        } else {
            return Ok(None);
        }
    }

    fn parse_increment_clause(&mut self) -> Result<Option<Expr>, ParseError> {
        if !self.matches(TokenType::RightParen) {
            let increment_clause = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
            return Ok(Some(increment_clause));
        } else {
            return Ok(None);
        }
    }

    fn if_statement(&mut self) -> Result<Declaration, ParseError> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;

        let then_branch = Box::new(self.statement()?);

        let else_branch = if self.matches(TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };

        let line = self.previous_token_line();
        let declaration = Declaration::if_statement(condition, then_branch, else_branch, line);
        Ok(declaration)
    }

    fn block(&mut self) -> Result<Declaration, ParseError> {
        let line = self.previous_token_line();
        let mut declarations = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.check(TokenType::Eof) {
            declarations.push(self.declaration()?);
        }

        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        let block = Declaration::block(declarations, line);
        Ok(block)
    }

    fn var_declaration(&mut self) -> Result<Declaration, ParseError> {
        let token = self.parse_variable("Expect variable name.")?;

        let initializer = if self.matches(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };

        self.consume(
            TokenType::Semicolon,
            "Expect ';' after variable declaration.",
        )?;
        let name = token.lexeme;
        let line = self.previous_token_line();
        let declaration = Declaration::variable_declaration(name, initializer, line);
        Ok(declaration)
    }

    fn parse_variable(&mut self, error_message: &str) -> Result<Token, ParseError> {
        let token = self.current.clone().unwrap();
        self.consume(TokenType::Identifier, error_message)?;
        Ok(token)
    }

    fn synchronize(&mut self) {
        self.panic_mode = false;

        while self.current.clone().unwrap().kind != TokenType::Eof {
            if self.previous.clone().unwrap().kind == TokenType::Semicolon {
                return;
            }
            match self.current.clone().unwrap().kind {
                TokenType::Class
                | TokenType::Fun
                | TokenType::Var
                | TokenType::For
                | TokenType::If
                | TokenType::While
                | TokenType::Print
                | TokenType::Return => {
                    return;
                }
                _ => (),
            }
            self.advance();
        }
    }

    fn expression_statement(&mut self) -> Result<Declaration, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        let line = self.previous_token_line();
        let declaration = Declaration::statement(Statement::ExprStatement(expr), line);
        Ok(declaration)
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

    fn print_statement(&mut self) -> Result<Declaration, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after value.")?;
        let line = self.previous_token_line();
        let declaration = Declaration::statement(Statement::PrintStatement(expr), line);
        Ok(declaration)
    }

    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        self.advance();

        if self.previous.is_none() {
            self.previous = self.current.clone();
        }

        let previous_token_type = self.previous.clone().unwrap().kind;
        let Some(prefix_rule) = get_rule(previous_token_type).prefix else {
            self.error("Expect expression.");
            return Err(ParseError::ExpectedExpression);
        };

        let can_assign = precedence <= Precedence::Assignment;
        let mut expr = prefix_rule(self, can_assign)?;

        while precedence <= get_rule(self.current.clone().unwrap().kind).precedence {
            self.advance();
            let infix_rule = get_rule(self.previous.clone().unwrap().kind).infix;
            if let Some(rule) = infix_rule {
                expr = rule(self, expr, can_assign)?;
            }
        }

        if can_assign && self.matches(TokenType::Equal) {
            self.error("Invalid assignment target.");
            return Err(ParseError::ExpectedExpression);
        }

        Ok(expr)
    }

    // ---------- Parsing rules ----------

    pub fn expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    fn number(&mut self, _can_assign: bool) -> Result<Expr, ParseError> {
        let value = self
            .previous
            .clone()
            .unwrap()
            .lexeme
            .parse::<f64>()
            .map_err(|_| ParseError::ExpectedNumber)?;
        Ok(Expr::Literal(Literal::Number(value)))
    }

    fn string(&mut self, _can_assign: bool) -> Result<Expr, ParseError> {
        let length = self.previous.clone().unwrap().length;
        // Remove quotes from string
        let string = &self.previous.clone().unwrap().lexeme[1..length - 1];
        Ok(Expr::Literal(Literal::String(string.into())))
    }

    fn variable(&mut self, can_assign: bool) -> Result<Expr, ParseError> {
        let variable_name = self.previous.clone().unwrap();
        self.named_variable(variable_name, can_assign)
    }

    fn named_variable(&mut self, name: Token, can_assign: bool) -> Result<Expr, ParseError> {
        if can_assign && self.matches(TokenType::Equal) {
            let value = self.expression()?;
            return Ok(Expr::VariableAssignment {
                name: name.lexeme,
                value: Box::new(value),
            });
        } else {
            return Ok(Expr::Variable { name: name.lexeme });
        }
    }

    fn unary(&mut self, _can_assign: bool) -> Result<Expr, ParseError> {
        let operator = self.previous.clone().unwrap();
        let right = self.parse_precedence(Precedence::Unary)?;
        Ok(Expr::Unary {
            operator,
            right: Box::new(right),
        })
    }

    fn binary(&mut self, left: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        let operator = self.previous.clone().unwrap();
        let rule = get_rule(operator.kind);
        let right = self.parse_precedence(rule.precedence.next())?;
        Ok(Expr::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn grouping(&mut self, _can_assign: bool) -> Result<Expr, ParseError> {
        let expr = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
        Ok(Expr::Grouping {
            expression: Box::new(expr),
        })
    }

    fn literal(&mut self, _can_assign: bool) -> Result<Expr, ParseError> {
        match &self.previous.clone().unwrap().kind {
            TokenType::False => Ok(Expr::Literal(Literal::Bool(false))),
            TokenType::True => Ok(Expr::Literal(Literal::Bool(true))),
            TokenType::Nil => Ok(Expr::Literal(Literal::Nil)),
            _ => unreachable!(),
        }
    }

    fn logic_and(&mut self, left: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        let operator = self.previous.clone().unwrap();
        let right = self.parse_precedence(Precedence::And.next())?;
        Ok(Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
    }

    fn logic_or(&mut self, left: Expr, _can_assign: bool) -> Result<Expr, ParseError> {
        let operator = self.previous.clone().unwrap();
        let right = self.parse_precedence(Precedence::Or.next())?;
        Ok(Expr::Logical {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        })
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

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<(), ParseError> {
        if let Some(token) = &self.current {
            if token.kind == token_type {
                self.advance();
                return Ok(());
            }
        }
        self.error(message);
        Err(ParseError::ExpectedToken(token_type, message.to_string()))
    }

    fn previous_token_line(&mut self) -> usize {
        self.previous.clone().unwrap().line
    }

    // ---------- Error handling ----------

    fn error(&mut self, message: &str) {
        if let Some(token) = &self.previous {
            let token = token.clone();
            self.error_at(&token, message);
        }
    }

    fn error_at(&mut self, token: &Token, message: &str) {
        if self.panic_mode {
            return;
        }
        self.panic_mode = true;
        eprint!("[line {}] Error", token.line);

        match token.kind {
            TokenType::Eof => eprint!(" at end"),
            TokenType::Error => (),
            _ => eprint!(" at '{}': {}", token.lexeme, message),
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
    prefix: Option<fn(&mut Parser, bool) -> Result<Expr, ParseError>>,
    infix: Option<fn(&mut Parser, Expr, bool) -> Result<Expr, ParseError>>,
    precedence: Precedence,
}

impl ParseRule {
    fn new(
        prefix: Option<fn(&mut Parser, bool) -> Result<Expr, ParseError>>,
        infix: Option<fn(&mut Parser, Expr, bool) -> Result<Expr, ParseError>>,
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
        TokenType::Identifier => ParseRule::new(Some(Parser::variable), None, Precedence::None),
        TokenType::And => ParseRule::new(None, Some(Parser::logic_and), Precedence::And),
        TokenType::Or => ParseRule::new(None, Some(Parser::logic_or), Precedence::Or),
        _ => ParseRule::new(None, None, Precedence::None),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::declaration::DeclarationKind;
    use crate::expr::Expr;
    use crate::scanner::Scanner;

    fn scan_and_parse(source: &str) -> Expr {
        let mut scanner = Scanner::new(source.into());
        let tokens = scanner.scan();
        let mut parser = Parser::new(tokens);
        parser.expression().unwrap()
    }

    fn scan_and_parse_declarations(source: &str) -> Vec<Declaration> {
        let mut scanner = Scanner::new(source.into());
        let tokens = scanner.scan();
        let mut parser = Parser::new(tokens);
        parser.parse()
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

    #[test]
    fn test_for_statement() {
        let source = "
        for (var i = 0; i < 5; i = i + 1) {
            print i;
        }
        ";
        let declarations = scan_and_parse_declarations(source);
        let declaration = declarations[0].clone();

        // Check the declaration is a for statement
        match declaration.inner {
            DeclarationKind::Statement(Statement::ForStatement {
                initializer_clause,
                condition_clause,
                increment_clause,
                body,
            }) => {
                // Check the initialized clause
                let expected_initializer_clause = Declaration::variable_declaration(
                    "i".into(),
                    Some(Expr::Literal(Literal::Number(0.0))),
                    2,
                );
                let initializer_clause = initializer_clause.unwrap();
                assert_eq!(initializer_clause, expected_initializer_clause);

                // Check the condition clause
                match condition_clause.unwrap() {
                    Expr::Binary {
                        left,
                        operator,
                        right,
                    } => {
                        let expected_left = Expr::Variable { name: "i".into() };
                        assert_eq!(*left, expected_left);

                        assert_eq!(operator.kind, TokenType::Less);

                        let expected_right = Expr::Literal(Literal::Number(5.0));
                        assert_eq!(*right, expected_right);
                    }
                    _ => panic!("Expected binary condition clause"),
                }

                // Check the increment clause
                match increment_clause.unwrap() {
                    Expr::VariableAssignment { name, value } => {
                        assert_eq!(name, "i".to_string());
                        match *value {
                            Expr::Binary {
                                left,
                                operator,
                                right,
                            } => {
                                let expected_left = Expr::Variable { name: "i".into() };
                                assert_eq!(*left, expected_left);

                                assert_eq!(operator.kind, TokenType::Plus);

                                let expected_right = Expr::Literal(Literal::Number(1.0));
                                assert_eq!(*right, expected_right);
                            }
                            _ => panic!(
                                "Expected binary expression as increment condition clause value"
                            ),
                        }
                    }
                    _ => panic!("Expected variable assignment as increment clause"),
                }

                match body.inner {
                    DeclarationKind::Block(declarations) => {
                        let expected_statement =
                            Statement::PrintStatement(Expr::Variable { name: "i".into() });
                        assert_eq!(
                            declarations,
                            vec![Declaration::statement(expected_statement, 3)]
                        )
                    }
                    _ => panic!("Expected block as for statement body"),
                }
            }
            _ => panic!("Expected for statement"),
        }
        dbg!(declarations);
    }
}
