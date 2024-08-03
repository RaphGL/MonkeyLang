mod builtins;
mod env;
mod interpreter;
mod lexer;
mod parser;

use env::{EnvCell, Environment};
use std::fs;
use std::io::{self, Write};
use std::rc::Rc;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn run_program(input: &str, env: EnvCell) {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let program = parser.parse();

    if let Some(program) = program {
        println!("{}", interpreter::eval(&program, Rc::clone(&env)));
    } else {
        for err in parser.errors {
            println!("{}", err);
        }
    }
}

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();
    let env = Environment::new();

    let mut args = std::env::args();
    if args.len() > 1 {
        let file = args.nth(1).unwrap();
        let code = fs::read_to_string(file).unwrap();
        run_program(&code, Rc::clone(&env));
        return;
    }

    loop {
        print!(">> ");
        stdout.flush().unwrap();

        input.clear();
        stdin.read_line(&mut input).unwrap();
        run_program(&input, Rc::clone(&env));
    }
}
