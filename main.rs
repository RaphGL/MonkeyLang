use std::io::{self, Write};
mod lexer;

use crate::lexer::{Lexer, Token};

fn main() {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    let mut input = String::new();

    loop {
        print!(">> ");
        stdout.flush().unwrap();

        input.clear();
        stdin.read_line(&mut input).unwrap();

        let mut lexer = Lexer::new(input.as_str());

        loop {
            let token = lexer.next_token();
            if matches!(token, Token::Eof) {
                break;
            }

            println!("{token:?}");
        }
    }
}
