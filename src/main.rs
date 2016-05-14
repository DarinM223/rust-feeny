#[macro_use]
extern crate log;
extern crate env_logger;

pub mod ast;
pub mod bytecode;
pub mod interpreter;
pub mod vm;

use std::env;

fn main() {
    env_logger::init().unwrap();

    let args: Vec<_> = env::args().collect();
    if args.len() > 2 {
        let opt_str = args[1].clone();
        let path = args[2].clone();

        match &opt_str[..] {
            "ast" => {
                let stmt = ast::read_ast(path).unwrap();
                stmt.print();

                println!("");

                match interpreter::interpret(stmt) {
                    Ok(_) => println!("Program completed!"),
                    Err(err) => println!("{:?}", err),
                }
            }
            "bc" => {
                let program = bytecode::load_bytecode(&path[..]).unwrap();
                program.print();
            }
            _ => panic!("Unknown option (allowed options: ast/bc)"),
        }
    } else {
        println!("You need to provide the option and the path to a file");
    }
}
