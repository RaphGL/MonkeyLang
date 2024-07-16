use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq)]
enum Expression {
    Int(i64),
    Ident(String),
    PrefixNeg(Box<Expression>),
    PrefixNot(Box<Expression>),
    Boolean(bool),
    Infix {
        left: Box<Expression>,
        op: Token,
        right: Box<Expression>,
    },
}

pub enum Statement {
    Let {
        name: Token,
        // value: Expression
    },
    Return, // (value: Expr);
    ExpressionStmt(Expression),
}

#[derive(Clone)]
enum Precedence {
    Lowest = 1,
    Equals,      // ==
    LessGreater, // > or <
    Sum,         // +
    Product,     // *
    Prefix,      // -X or !X
    Call,        // myFunction(X)
}

impl From<Token> for Precedence {
    fn from(value: Token) -> Self {
        match value {
            Token::Eq => Self::Equals,
            Token::NotEq => Self::Equals,
            Token::LT => Self::LessGreater,
            Token::GT => Self::LessGreater,
            Token::Plus => Self::Sum,
            Token::Minus => Self::Sum,
            Token::Slash => Self::Product,
            Token::Asterisk => Self::Product,
            _ => Self::Lowest,
        }
    }
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
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_prefix(&mut self) -> Option<Expression> {
        let expr = match self.curr_token.clone() {
            Token::Ident(ident) => Expression::Ident(ident),
            Token::Int(int) => {
                let Ok(int) = int.parse::<i64>() else {
                    return None;
                };

                Expression::Int(int)
            }

            Token::Bang | Token::Minus => {
                let token = self.curr_token.clone();
                self.next_token();
                let Some(expr) = self.parse_expression(Precedence::Prefix) else {
                    return None;
                };

                match token {
                    Token::Bang => Expression::PrefixNot(expr.into()),
                    Token::Minus => Expression::PrefixNeg(expr.into()),
                    _ => unreachable!(),
                }
            }

            token @ (Token::True | Token::False) => Expression::Boolean(token == Token::True),

            token @ _ => {
                self.errors
                    .push(format!("no prefix parsing available for {token:?}"));
                return None;
            }
        };

        Some(expr)
    }

    fn parse_infix(&mut self, left: Expression) -> Option<Expression> {
        let op_token = self.curr_token.clone();
        let precedence = Precedence::from(op_token.clone());
        self.next_token();
        let right = self.parse_expression(precedence)?;

        Some(Expression::Infix {
            left: Box::new(left),
            op: op_token,
            right: Box::new(right),
        })
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let mut left = self.parse_prefix()?;

        let precedence = precedence as u32;
        while self.next_token != Token::Semicolon
            && precedence < Precedence::from(self.next_token.clone()) as u32
        {
            self.next_token();
            left = self.parse_infix(left)?;
        }

        Some(left)
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.next_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::ExpressionStmt(expr))
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
            expr: Expression,
        }

        let prefixes = [
            Prefix {
                input: "!5",
                operator: "!",
                expr: Expression::Int(5),
            },
            Prefix {
                input: "-15",
                operator: "-",
                expr: Expression::Int(15),
            },
            Prefix {
                input: "!true",
                operator: "!",
                expr: Expression::Boolean(true),
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

            assert_eq!(prefix.expr, **expr);
        }
    }

    #[test]
    fn infix_expressions() {
        struct Infix<'a> {
            input: &'a str,
            left: i64,
            op: Token,
            right: i64,
        }

        let inputs = [
            Infix {
                input: "5 + 5",
                left: 5,
                op: Token::Plus,
                right: 5,
            },
            Infix {
                input: "5 - 5",
                left: 5,
                op: Token::Minus,
                right: 5,
            },
            Infix {
                input: "5 * 5",
                left: 5,
                op: Token::Asterisk,
                right: 5,
            },
            Infix {
                input: "5 / 5",
                left: 5,
                op: Token::Slash,
                right: 5,
            },
            Infix {
                input: "5 > 5",
                left: 5,
                op: Token::GT,
                right: 5,
            },
            Infix {
                input: "5 < 5",
                left: 5,
                op: Token::LT,
                right: 5,
            },
            Infix {
                input: "5 == 5",
                left: 5,
                op: Token::Eq,
                right: 5,
            },
            Infix {
                input: "5 != 5",
                left: 5,
                op: Token::NotEq,
                right: 5,
            },
        ];

        for Infix {
            input,
            left,
            op,
            right,
        } in inputs
        {
            let program = new_program(input).unwrap();

            if program.len() != 1 {
                panic!();
            }

            let Statement::ExpressionStmt(ref stmt) = program[0] else {
                panic!();
            };

            let Expression::Infix {
                left: found_left,
                op: found_op,
                right: found_right,
            } = stmt
            else {
                panic!();
            };

            let Expression::Int(found_left) = **found_left else {
                panic!();
            };

            let Expression::Int(found_right) = **found_right else {
                panic!();
            };

            assert_eq!(found_left, left);
            assert_eq!(found_right, right);
            assert_eq!(*found_op, op);
        }
    }

    #[test]
    fn operator_precedence() {
        struct Test<'a> {
            input: &'a str,
            expected: &'a str,
        }

        todo!("found on page 67 and 80 of writing go interpreter book")
    }

    #[test]
    fn boolean_literal() {
        struct Infix<'a> {
            input: &'a str,
            left: bool,
            op: Token,
            right: bool,
        }

        let inputs = [
            Infix {
                input: "true == true",
                left: true,
                op: Token::Eq,
                right: true,
            },
            Infix {
                input: "true != false",
                left: true,
                op: Token::NotEq,
                right: false,
            },
            Infix {
                input: "false == false",
                left: false,
                op: Token::Eq,
                right: false,
            },
        ];

        for input in inputs {
            let program = new_program(input.input).unwrap();

            let Statement::ExpressionStmt(ref expr) = program[0] else {
                panic!();
            };

            let Expression::Infix { left, op, right } = expr else {
                panic!();
            };

            let Expression::Boolean(left) = **left else {
                panic!();
            };

            let Expression::Boolean(right) = **right else {
                panic!();
            };

            assert_eq!(input.left, left);
            assert_eq!(input.right, right);
            assert_eq!(input.op, *op);
        }
    }
}
