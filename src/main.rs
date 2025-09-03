fn main() {
    let args: Vec<String> = std::env::args().collect();

    if args.len() == 1 {
        // TODO: Implement REPL
    } else if args.len() == 2 {
        // TODO: Implement file reading and execution
    } else {
        eprintln!("Usage: rlox [path]");
        std::process::exit(64);
    }
}
