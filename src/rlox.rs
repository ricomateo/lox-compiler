use std::fs;
use std::io::{self, Write};

use crate::{compiler::Compiler, parser::Parser, scanner::Scanner, vm::Vm};

pub struct Rlox;

impl Rlox {
    pub fn new() -> Self {
        Rlox
    }

    pub fn main(&mut self) {
        let args: Vec<String> = std::env::args().collect();

        if args.len() == 1 {
            self.repl();
        } else if args.len() == 2 {
            self.run_file(&args[1]);
        } else {
            eprintln!("Usage: rlox [path]");
            std::process::exit(64);
        }
    }

    fn repl(&mut self) {
        let stdin = io::stdin();

        loop {
            print!("> ");
            io::stdout().flush().unwrap();

            let mut line = String::new();

            if stdin.read_line(&mut line).unwrap() == 0 {
                println!();
                break;
            }

            self.interpret(line);
        }
    }

    fn run_file(&mut self, path: &str) {
        let source = fs::read_to_string(path).unwrap_or_else(|_| {
            eprintln!("Could not read file: {}", path);
            std::process::exit(65);
        });
        self.interpret(source);
    }

    fn interpret(&mut self, source: String) {
        log::info!("Source: {:#?}", source);

        // Phase 1: Scanning
        log::info!("Scanning...");
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan();
        log::debug!("Tokens: {:#?}", tokens);

        // Phase 2: Parsing
        log::info!("Parsing...");
        let mut parser = Parser::new(tokens);
        let declarations = parser.parse();
        // Don't compile the declarations if there were parsing errors
        if parser.had_error {
            return;
        }
        log::debug!("Declarations: {:#?}", declarations);

        // Phase 3: Compilation
        log::info!("Compiling...");
        let mut compiler = Compiler::new();
        let chunk = compiler.compile(&declarations);

        // Phase 4: Running
        log::info!("Running...");
        let mut vm = Vm::new(chunk);
        let result = vm.run();
        if result.is_err() {
            std::process::exit(65);
        }
    }
}
