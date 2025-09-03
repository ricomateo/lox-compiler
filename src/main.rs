use std::io::{self, Write};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        // TODO: Implement file reading and execution
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }
}

fn repl() {
    let stdin = io::stdin();

    loop {
        print!("> ");
        io::stdout().flush().unwrap();

        let mut line = String::new();
        
        if stdin.read_line(&mut line).unwrap() == 0 {
            println!();
            break;
        }

        // TODO: Implement interpret function
    }
}