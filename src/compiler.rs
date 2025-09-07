use crate::scanner::{Scanner, TokenType};

pub fn compile(source: &str) {
    let mut scanner = Scanner::new(source);

    let mut line: isize = -1;

    loop {
        let token = scanner.scan_token();

        if token.line as isize != line {
            print!("{:4} ", token.line);
            line = token.line as isize;
        } else {
            print!("   | ");
        }

        println!("{:2?} '{}'", token.kind as u8, &scanner.source[token.start..token.start + token.lenght]);

        if token.kind == TokenType::Eof {
            break;
        }
    }
}
