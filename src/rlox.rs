use std::fs;
use std::io::{self, Write};

use crate::{compiler::compile, scanner::Scanner, vm::Vm};

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
        // Phase 1: Scanning
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan();

        // Phase 2: Parsing and compiling (for now, we do it in one step)
        let chunk = compile(tokens).unwrap();

        // Phase 3: Compiling
        // TODO: Separate compilation (bytecode generation) from parsing

        // Phase 4: Running
        let mut vm = Vm::new(chunk);
        vm.run().unwrap();
    }
}
