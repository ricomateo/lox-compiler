use crate::value::Value;
extern crate libc;
use std::io;

// We need to call C code because there is no clock function in Rust
unsafe extern "C" {
    fn clock() -> ::libc::clock_t;
}

pub fn clock_native(_arg_count: usize, _args: Vec<Value>) -> Value {
    let clock = unsafe { clock() } as f64;
    Value::Number(clock)
}

pub fn input_native(_arg_count: usize, args: Vec<Value>) -> Value {
    let Value::String(prompt) = &args[0] else {
        panic!("Expected prompt argument at input() function");
    };
    println!("{}", prompt);
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    Value::String(input.trim().to_string())
}
