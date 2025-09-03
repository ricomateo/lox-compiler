use crate::compiler::compile;

pub enum InterpretResult {
    Ok,
}

pub fn interpret(source: &str) -> InterpretResult {
    compile(source);
    InterpretResult::Ok
}