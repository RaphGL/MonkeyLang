use crate::lexer::{Lexer, Token};

#[derive(Debug)]
enum Expression {
    Int(i64),
    Ident(String),
    PrefixNeg(Box<Expression>),
    PrefixNot(Box<Expression>),
}

pub enum Statement {
    Let {
        name: Token,
        // value: Expression
    },
    Return, // (value: Expr);
    ExpressionStmt(Expression),
}

enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

type Program<'a> = Vec<Statement>;

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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_prefix(&mut self) -> Option<Statement> {
        let stmt = match self.curr_token {
            Token::Ident(ref ident) => Statement::ExpressionStmt(Expression::Ident(ident.clone())),
            Token::Int(ref int) => {
                let Ok(int) = int.parse::<i64>() else {
                    return None;
                };

                Statement::ExpressionStmt(Expression::Int(int))
            }

            Token::Bang | Token::Minus => {
                let token = self.curr_token.clone();
                self.next_token();
                let Some(Statement::ExpressionStmt(expr)) =
                    self.parse_expression(Precedence::Prefix)
                else {
                    return None;
                };

                Statement::ExpressionStmt(match token {
                    Token::Bang => Expression::PrefixNot(expr.into()),
                    Token::Minus => Expression::PrefixNeg(expr.into()),
                    _ => unreachable!(),
                })
            }

            ref token @ _ => {
                self.errors
                    .push(format!("no prefix parsing available for {token:?}"));
                return None;
            }
        };

        Some(stmt)
    }
    fn parse_infix(&mut self) {
        todo!()
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Statement> {
        let prefix = self.parse_prefix();
        prefix
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.next_token == Token::Semicolon {
            self.next_token();
        }

        Some(expr)
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

    fn new_program(input: &str) -> Option<Program> {
        let lexer = Lexer::new(input);
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        check_parser_errors(&parser);
        program
    }

    #[test]
    fn let_statements() {
        let input = r#"
           let x = 5;
           let y = 10;
           let foobar = 838383; 
        "#;

        let program = new_program(input).unwrap();
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

        let program = new_program(input).unwrap();
        if program.len() != 3 {
            panic!();
        }

        for stmt in program {
            let Statement::Return = stmt else {
                panic!();
            };
        }
    }

    #[test]
    fn identifier_expression() {
        let input = "foobar";

        let program = new_program(input).unwrap();
        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(token) = &program[0] else {
            panic!()
        };

        let Expression::Ident(name) = token else {
            panic!();
        };

        assert_eq!(name, input);
    }

    #[test]
    fn integer_literal_expression() {
        let input = "5";

        let program = new_program(input).unwrap();
        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(token) = &program[0] else {
            panic!()
        };

        let Expression::Int(name) = *token else {
            panic!();
        };

        assert_eq!(name, input.parse::<i64>().unwrap());
    }

    #[test]
    fn prefix_expressions() {
        struct Prefix<'a> {
            input: &'a str,
            operator: &'a str,
            int_val: i64,
        }

        let prefixes = [
            Prefix {
                input: "!5",
                operator: "!",
                int_val: 5,
            },
            Prefix {
                input: "-15",
                operator: "-",
                int_val: 15,
            },
        ];

        for prefix in prefixes {
            let program = new_program(prefix.input).unwrap();

            if program.len() != 1 {
                panic!();
            }

            let Statement::ExpressionStmt(ref token) = program[0] else {
                panic!();
            };

            let expr = match token {
                Expression::PrefixNeg(expr) => {
                    assert_eq!(prefix.operator, "-");
                    expr
                }
                Expression::PrefixNot(expr) => {
                    assert_eq!(prefix.operator, "!");
                    expr
                }

                _ => panic!(),
            };

            if let Expression::Int(int) = **expr {
                assert_eq!(prefix.int_val, int);
            } else {
                panic!();
            }
        }
    }
}
