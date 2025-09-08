use std::io::{self, Write};

use lox_compiler::{
    chunk::Chunk,
    vm::Vm,
};

fn main() {
    let chunk = Chunk::new();
    let vm = Vm::new(chunk);

    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl(vm);
    } else if args.len() == 2 {
        run_file(&args[1]);
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }
}

fn repl(mut vm: Vm) {
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        
        if stdin.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }

        let _ = vm.interpret(&line);
    }
}

fn run_file(path: &str) {
    let _source = std::fs::read_to_string(path)
        .unwrap_or_else(|_| {
            eprintln!("Could not read file: {}", path);
            std::process::exit(65);
        });

    // TODO: interpret(&source);
}
