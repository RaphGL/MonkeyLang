use std::io::{self, Write};
mod lexer;
mod parser;

use crate::lexer::Lexer;
use crate::parser::Parser;

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        print!(">> ");
        stdout.flush().unwrap();

        input.clear();
        stdin.read_line(&mut input).unwrap();

        let lexer = Lexer::new(input.as_str());
        let mut parser = Parser::new(lexer);
        let program = parser.parse();

        if let Some(program) = program {
            println!("{:?}", program);
        } else {
            for err in parser.errors {
                println!("{}", err);
            }
        }
    }
}
