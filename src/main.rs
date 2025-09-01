use lox_compiler::chunk::{Chunk, OpCode, Value};

fn main() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(Value::Number(1.2));
    let line = 123;
    chunk.write(OpCode::Constant(constant_index), line);
    chunk.write(OpCode::Return, line);

    chunk.disassemble("test chunk");
}
