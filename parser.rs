use crate::lexer::{Lexer, Token};

pub enum Statement {
    Let {
        name: Token,
        // value: Expression
    },

    Return, // (value: Expr);
}

type Program = Vec<Statement>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    next_token: Token,
    errors: Vec<String>,
}

impl<'a> Parser<'a> {
    pub fn new(mut lexer: Lexer<'a>) -> Self {
        let curr_token = lexer.next_token();
        let next_token = lexer.next_token();

        Self {
            lexer,
            curr_token,
            next_token,
            errors: Vec::new(),
        }
    }

    fn next_token(&mut self) {
        self.curr_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn peek_error(&mut self, expected_token: Token) {
        self.errors.push(format!(
            "expected next token to be {expected_token:?} found {:?}",
            self.curr_token
        ));
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let Token::Ident(name) = self.next_token.clone() else {
            self.peek_error(Token::Ident("".into()));
            return None;
        };

        self.next_token();

        let Token::Assign = self.next_token else {
            self.peek_error(Token::Assign);
            return None;
        };

        self.next_token();

        // todo: handle expressions
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let {
            name: Token::Ident(name),
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        // todo: handle expressions
        while self.curr_token != Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return)
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => None,
        }
    }

    pub fn parse(&mut self) -> Option<Program> {
        let mut program = Program::new();

        while self.curr_token != Token::Eof {
            let stmt = self.parse_statement();
            program.push(stmt?);
            self.next_token();
        }

        Some(program)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn check_parser_errors(parser: &Parser) {
        for error in &parser.errors {
            eprintln!("{error}");
        }

        if !parser.errors.is_empty() {
            panic!();
        }
    }

    #[test]
    fn let_statements() {
        let input = r#"
           let x = 5;
           let y = 10;
           let foobar = 838383; 
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&parser);

        let program = program.unwrap();
        if program.len() != 3 {
            panic!();
        }

        let expected_identifiers = ["x", "y", "foobar"];
        for (stmt, expected) in program.iter().zip(expected_identifiers) {
            if let Statement::Let { name: ident_token } = stmt {
                let Token::Ident(name) = ident_token else {
                    panic!("expected identifier token found.");
                };

                assert_eq!(name, expected);
            }
        }
    }

    #[test]
    fn return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&parser);

        let program = program.unwrap();

        for stmt in program {
            let Statement::Return = stmt else {
                panic!();
            };
        }
    }
}
