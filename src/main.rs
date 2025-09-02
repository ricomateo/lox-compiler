use lox_compiler::{
    chunk::{Chunk, OpCode, Value},
    vm::Vm,
};

fn main() {
    let mut chunk = Chunk::new();
    let constant_index = chunk.add_constant(Value::Number(1.2));
    let line = 123;
    chunk.write(OpCode::Constant(constant_index), line);
    chunk.write(OpCode::Negate, line);
    chunk.write(OpCode::Return, line);

    let mut vm = Vm::new(chunk);
    vm.interpret().unwrap();
    // chunk.disassemble("test chunk");
}
