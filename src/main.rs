#![feature(convert)]

pub mod ast;
pub mod interpreter;

use std::env;

fn main() {
    if let Some(arg) = env::args().nth(1) {
        if let Ok(stmt) = ast::read_ast(arg) {
            stmt.print();
        }
    } else {
        println!("You need to provide the path to the AST file");
    }
}
