use lox_compiler::rlox::Rlox;

fn main() {
    env_logger::init();

    let mut rlox = Rlox::new();
    rlox.main();
}
