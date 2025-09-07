use std::io::{self, Write};

fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        repl();
    } else if args.len() == 2 {
        run_file(&args[1]);
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

        // TODO: interpret(&line);
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
