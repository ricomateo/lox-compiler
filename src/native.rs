use crate::value::Value;
extern crate libc;
use rand::Rng;
use std::io;

// We need to call C code because there is no clock function in Rust
unsafe extern "C" {
    fn clock() -> ::libc::clock_t;
}

pub fn clock_native(_arg_count: usize, _args: Vec<Value>) -> Value {
    let clock = unsafe { clock() } as f64;
    Value::Number(clock)
}

/// Reads user input
pub fn input_native(_arg_count: usize, args: Vec<Value>) -> Value {
    let Some(Value::String(prompt)) = args.get(0) else {
        panic!("Expected prompt argument at input() function");
    };
    println!("{}", prompt);
    let mut input = String::new();
    io::stdin().read_line(&mut input).unwrap();
    Value::String(input.trim().to_string())
}

/// Takes two arguments `from` and `to`, and generates a random number
/// within that range.
pub fn random_native(_arg_count: usize, args: Vec<Value>) -> Value {
    let Some(Value::Number(from)) = args.get(0) else {
        panic!("Expected 'from' argument in random() function");
    };
    let Some(Value::Number(to)) = args.get(1) else {
        panic!("Expected 'to' argument in random function");
    };
    let from = *from as usize;
    let to = *to as usize;
    let num = rand::rng().random_range(from..to) as f64;
    Value::Number(num)
}

/// Takes a string and converts it to integer
pub fn int_native(_arg_count: usize, args: Vec<Value>) -> Value {
    let Some(Value::String(string)) = args.get(0) else {
        panic!("Expected argument in int() function");
    };
    let Ok(number) = string.parse::<usize>() else {
        panic!("Cannot convert '{string}' to integer");
    };
    Value::Number(number as f64)
}
