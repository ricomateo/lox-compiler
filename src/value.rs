use crate::chunk::Chunk;

#[derive(Debug, Clone, PartialEq)]
#[allow(unpredictable_function_pointer_comparisons)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Nil,
    String(String),
    Function(Function),
    NativeFunction(NativeFunction),
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Function {
    pub arity: usize,
    pub chunk: Chunk,
    pub name: String,
}

/// A native function can be represented as a Rust function that takes:
///  - an usize (number of arguments)
///  - a Vec<Value> (the actual arguments)
/// and returns a Value.
pub type NativeFunction = fn(usize /* arg_count */, Vec<Value> /* args */) -> Value;
