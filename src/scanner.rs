/// In the book, the Scanner uses raw pointers to the start and current positions in the source string.
/// In our implementation, we represent those positions as indices into the source string (&str).

/// The 'source' field holds a reference to the entire input string with a lifetime `'a`.
/// This ensures that the Scanner cannot outlive the source string it is scanning.
pub struct Scanner<'a> {
    /// The full source code to scan
    pub source: &'a str,
    /// Index where the curent token begins
    start: usize,
    /// Index of the character being currently scanned
    current: usize,
    /// Current line number
    line: usize,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals
    Identifier,
    String,
    Number,

    // Keywords
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Error,
    Eof,
}

/// In the book, Token stores a pointer to the start of the lexeme and its length.
/// In our implementation, we store the start as an index an the lenght separately. To get the lexeme, we slice the source string using these indices.
#[derive(Debug)]
pub struct Token {
    /// Type of the token
    pub kind: TokenType,
    /// Index where the lexeme starts 
    pub start: usize,
    /// Length of the lexeme
    pub length: usize,
    /// Line number of the token
    pub line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            start: 0,
            current: 0,
            line: 1,
        }
    }

    /// Scans the next token from the source code and returns it.
    pub fn scan_token(&mut self) -> Token {
        self.skip_whitespace();

        self.start = self.current;

        if self.is_at_end() {
            return self.make_token(TokenType::Eof);
        }

        let c = self.advance();

        if Self::is_alpha(c) {
            return self.identifier();
        }

        if Self::is_digit(c) {
            return self.number();
        }

        match c {
            // Single-character tokens
            '(' => self.make_token(TokenType::LeftParen),
            ')' => self.make_token(TokenType::RightParen),
            '{' => self.make_token(TokenType::LeftBrace),
            '}' => self.make_token(TokenType::RightBrace),
            ';' => self.make_token(TokenType::Semicolon),
            ',' => self.make_token(TokenType::Comma),
            '.' => self.make_token(TokenType::Dot),
            '-' => self.make_token(TokenType::Minus),
            '+' => self.make_token(TokenType::Plus),
            '/' => self.make_token(TokenType::Slash),
            '*' => self.make_token(TokenType::Star),
            // One or two character tokens
            '!' => {
                if self.match_char('=') {
                    self.make_token(TokenType::BangEqual)
                } else {
                    self.make_token(TokenType::Bang)
                }
            }
            '=' => {
                if self.match_char('=') {
                    self.make_token(TokenType::EqualEqual)
                } else {
                    self.make_token(TokenType::Equal)
                }
            }
            '<' => {
                if self.match_char('=') {
                    self.make_token(TokenType::LessEqual)
                } else {
                    self.make_token(TokenType::Less)
                }
            }
            '>' => {
                if self.match_char('=') {
                    self.make_token(TokenType::GreaterEqual)
                } else {
                    self.make_token(TokenType::Greater)
                }
            }
            // Literals
            '"' => self.string(),
            _ => self.error_token("Unexpected character"),
        }
    }

    /// Returns true if the scanner has reached the end of the source string.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Creates a token of the given type, using the current start and current indices to determine the lexeme.
    fn make_token(&self, kind: TokenType) -> Token {
        Token {
            kind,
            start: self.start,
            length: self.current - self.start,
            line: self.line,
        }
    }

    /// Creates an error token with the given message.
    fn error_token(&self, message: &'a str) -> Token {
        Token {
            kind: TokenType::Error,
            start: self.start,
            length: message.len(),
            line: self.line,
        }
    }

    /// Advances the scanner to the next character and returns the current one.
    fn advance(&mut self) -> char {
        let c = self.source[self.current..].chars().next().unwrap();
        self.current += c.len_utf8();
        c
    }

    /// If the current character matches the expected one, advances the scanner and returns true.
    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }

        let c = self.source[self.current..].chars().next().unwrap();

        if c != expected {
            return false;
        }

        self.current += c.len_utf8();
        true
    }

    /// Skips whitespace and comments in the source code.
    fn skip_whitespace(&mut self) {
        loop {
            let c = self.peek();

            match c {
                ' ' | '\r' | '\t' => {
                    self.advance();
                }
                '\n' => {
                    self.line += 1;
                    self.advance();
                }
                '/' => {
                    if self.peek_next() == '/' {
                        while !self.is_at_end() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else {
                        return;
                    }
                }
                _ => return,
            }
        }
    }

    /// Returns the current character without advancing the scanner.
    /// 
    /// In the book, this is done by deferencing the current pointer.
    /// In our implementation, we safely slice the string and return '\0' if we are at the end.
    fn peek(&self) -> char {
        self.source[self.current..].chars().next().unwrap_or('\0')
    }

    /// Returns the next character without advancing the scanner.
    /// 
    /// In the book, this uses pointer arithmetic to look one character ahead.
    /// In our implementation, we use a 'chars()' iterator and skip the first character safely.
    fn peek_next(&self) -> char {
        let mut iter = self.source[self.current..].chars();
        iter.next();
        iter.next().unwrap_or('\0')
    }

    /// Scans a string literal and returns the corresponding token.
    fn string(&mut self) -> Token {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return self.error_token("Unterminated string");
        }

        self.advance();
        self.make_token(TokenType::String)
    }

    /// Returns true if the character is a digit (0-9).
    fn is_digit(c: char) -> bool {
        c >= '0' && c <= '9'
    }

    /// Scans a number literal and returns the corresponding token.
    fn number(&mut self) -> Token {
        while Self::is_digit(self.peek()) {
            self.advance();
        }

        if self.peek() == '.' && Self::is_digit(self.peek_next()) {
            self.advance();

            while Self::is_digit(self.peek()) {
                self.advance();
            }
        }

        self.make_token(TokenType::Number)
    }

    fn is_alpha(c: char) -> bool {
        (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_'
    }

    /// Scans an identifier or a reserved keyword and returns the corresponding token.
    fn identifier(&mut self) -> Token {
        while Self::is_alpha(self.peek()) || Self::is_digit(self.peek()) {
            self.advance();
        }

        self.make_token(self.identifier_type())
    }

    /// Determines the type of the identifier by checking if it matches any reserved keywords.
    fn identifier_type(&self) -> TokenType {
        let text = &self.source[self.start..self.current];

        match text.chars().next().unwrap() {
            'a' => self.check_keyword(1, 2, "nd", TokenType::And),
            'c' => self.check_keyword(1, 4, "lass", TokenType::Class),
            'e' => self.check_keyword(1, 3, "lse", TokenType::Else),
            'f' => {
                if self.current - self.start > 1 {
                    return match text.chars().nth(1).unwrap() {
                        'a' => self.check_keyword(2, 3, "lse", TokenType::False),
                        'o' => self.check_keyword(2, 1, "r", TokenType::For),
                        'u' => self.check_keyword(2, 1, "n", TokenType::Fun),
                        _ => TokenType::Identifier,
                    };
                }
                return TokenType::Identifier;
            }
            'i' => self.check_keyword(1, 1, "f", TokenType::If),
            'n' => self.check_keyword(1, 2, "il", TokenType::Nil),
            'o' => self.check_keyword(1, 1, "r", TokenType::Or),
            'p' => self.check_keyword(1, 4, "rint", TokenType::Print),
            'r' => self.check_keyword(1, 5, "eturn", TokenType::Return),
            's' => self.check_keyword(1, 4, "uper", TokenType::Super),
            't' => {
                if self.current - self.start > 1 {
                    return match text.chars().nth(1).unwrap() {
                        'h' => self.check_keyword(2, 2, "is", TokenType::This),
                        'r' => self.check_keyword(2, 2, "ue", TokenType::True),
                        _ => TokenType::Identifier,
                    };
                }
                return TokenType::Identifier;
            }
            'v' => self.check_keyword(1, 2, "ar", TokenType::Var),
            'w' => self.check_keyword(1, 4, "hile", TokenType::While),
            _ => TokenType::Identifier,
        }
    }

    /// Checks if the current identifier matches a specific keyword.
    fn check_keyword(&self, start: usize, length: usize, rest: &str, kind: TokenType) -> TokenType {
        if self.current - self.start == start + length {
            if &self.source[self.start + start..self.start + start + length] == rest {
                return kind;
            }
        }

        TokenType::Identifier
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scanner::TokenType;

    fn scan_all_tokens(source: &str) -> Vec<TokenType> {
        let mut scanner = Scanner::new(source);
        let mut tokens = Vec::new();

        loop {
            let token = scanner.scan_token();
            tokens.push(token.kind);
            if token.kind == TokenType::Eof {
                break;
            }
        }

        tokens
    }

    #[test]
    fn test_single_char_tokens() {
        let source = "(){}.,-+;/*";

        let tokens = scan_all_tokens(source);

        assert_eq!(
            tokens,
            vec![
                TokenType::LeftParen,
                TokenType::RightParen,
                TokenType::LeftBrace,
                TokenType::RightBrace,
                TokenType::Dot,
                TokenType::Comma,
                TokenType::Minus,
                TokenType::Plus,
                TokenType::Semicolon,
                TokenType::Slash,
                TokenType::Star,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_one_or_two_char_tokens() {
        let source = "!= == <= >= = < > !";

        let tokens = scan_all_tokens(source);

        assert_eq!(
            tokens,
            vec![
                TokenType::BangEqual,
                TokenType::EqualEqual,
                TokenType::LessEqual,
                TokenType::GreaterEqual,
                TokenType::Equal,
                TokenType::Less,
                TokenType::Greater,
                TokenType::Bang,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_identifiers_and_keywords() {
        let source = "and class else false fun for if nil or print return super this true var while identifier";

        let tokens = scan_all_tokens(source);

        assert_eq!(
            tokens,
            vec![
                TokenType::And,
                TokenType::Class,
                TokenType::Else,
                TokenType::False,
                TokenType::Fun,
                TokenType::For,
                TokenType::If,
                TokenType::Nil,
                TokenType::Or,
                TokenType::Print,
                TokenType::Return,
                TokenType::Super,
                TokenType::This,
                TokenType::True,
                TokenType::Var,
                TokenType::While,
                TokenType::Identifier,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_numbers() {
        let source = "107585 10.7585";

        let tokens = scan_all_tokens(source);

        assert_eq!(
            tokens,
            vec![
                TokenType::Number,
                TokenType::Number,
                TokenType::Eof,
            ]
        );
    }

    #[test]
    fn test_strings() {
        let source = "\"Crafting\" \"Interpreters\"";

        let tokens = scan_all_tokens(source);
        
        assert_eq!(
            tokens,
            vec![
                TokenType::String,
                TokenType::String,
                TokenType::Eof,
            ]
        );
    }
}