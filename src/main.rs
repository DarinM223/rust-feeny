#![feature(convert)]

#[macro_use]
extern crate log;
extern crate env_logger;

pub mod ast;
pub mod bytecode;
pub mod interpreter;

use std::env;

fn main() {
    env_logger::init().unwrap();

    if let Some(arg) = env::args().nth(1) {
        if let Ok(stmt) = ast::read_ast(arg) {
            stmt.print();

            println!("");

            match interpreter::interpret(stmt) {
                Ok(_) => println!("Program completed!"),
                Err(err) => println!("{:?}", err),
            }
        }
    } else {
        println!("You need to provide the path to the AST file");
    }
}
