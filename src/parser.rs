use crate::lexer::{Lexer, Token};

#[derive(Debug, PartialEq)]
pub enum Expression {
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

    If {
        condition: Box<Expression>,
        consequence: Box<Statement>,
        alternative: Option<Box<Statement>>,
    },

    Function {
        params: Option<Vec<String>>,
        body: Box<Statement>,
    },

    Call {
        function: Box<Expression>,
        args: Option<Vec<Expression>>,
    },
}

#[derive(Debug, PartialEq)]
pub enum Statement {
    Let { name: Token, value: Expression },
    Return(Option<Expression>),
    ExpressionStmt(Expression),
    Block(Vec<Statement>),
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
            Token::LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}

pub type Program = Vec<Statement>;

pub struct Parser<'a> {
    lexer: Lexer<'a>,
    curr_token: Token,
    next_token: Token,
    pub errors: Vec<String>,
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
            self.next_token
        ));
    }

    fn parse_let_statement(&mut self) -> Option<Statement> {
        let Token::Ident(name) = self.next_token.clone() else {
            self.peek_error(Token::Ident("".into()));
            return None;
        };

        self.next_token();

        if self.next_token != Token::Assign {
            self.peek_error(Token::Assign);
            return None;
        };

        self.next_token();
        self.next_token();

        let expr = self.parse_expression(Precedence::Lowest)?;

        if self.next_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Let {
            name: Token::Ident(name),
            value: expr,
        })
    }

    fn parse_return_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let expr = if self.curr_token == Token::Semicolon {
            None
        } else {
            Some(self.parse_expression(Precedence::Lowest)?)
        };

        if self.next_token == Token::Semicolon {
            self.next_token();
        }

        Some(Statement::Return(expr))
    }

    fn parse_block_statement(&mut self) -> Option<Statement> {
        self.next_token();

        let mut block = Vec::new();

        while self.curr_token != Token::RBrace && self.curr_token != Token::Eof {
            let stmt = self.parse_statement();
            if let Some(stmt) = stmt {
                block.push(stmt);
            }

            self.next_token();
        }

        Some(Statement::Block(block))
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.curr_token {
            Token::Let => self.parse_let_statement(),
            Token::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_if_expression(&mut self) -> Option<Expression> {
        if self.next_token != Token::LParen {
            self.peek_error(Token::LParen);
            return None;
        }

        self.next_token();
        self.next_token();

        let condition = self.parse_expression(Precedence::Lowest)?;

        if self.next_token != Token::RParen {
            self.peek_error(Token::RParen);
            return None;
        }
        self.next_token();

        if self.next_token != Token::LBrace {
            self.peek_error(Token::LBrace);
            return None;
        }
        self.next_token();

        let consequence = self.parse_block_statement()?;

        let alternative = if self.next_token == Token::Else {
            self.next_token();

            if self.next_token != Token::LBrace {
                self.peek_error(Token::LBrace);
                return None;
            }
            self.next_token();

            Some(Box::new(self.parse_block_statement()?))
        } else {
            None
        };

        Some(Expression::If {
            condition: Box::new(condition),
            consequence: Box::new(consequence),
            alternative,
        })
    }

    fn parse_fn_params(&mut self) -> Option<Vec<String>> {
        let mut idents = Vec::new();

        if self.next_token == Token::RParen {
            self.next_token();
            return None;
        }
        self.next_token();

        let Token::Ident(curr_ident) = self.curr_token.clone() else {
            return None;
        };
        idents.push(curr_ident);

        while self.next_token == Token::Comma {
            self.next_token();
            self.next_token();
            let Token::Ident(curr_ident) = self.curr_token.clone() else {
                return None;
            };
            idents.push(curr_ident);
        }

        if self.next_token != Token::RParen {
            None
        } else {
            self.next_token();
            Some(idents)
        }
    }

    fn parse_fn_literal(&mut self) -> Option<Expression> {
        if self.next_token != Token::LParen {
            return None;
        }
        self.next_token();

        let params = self.parse_fn_params();

        if self.next_token != Token::LBrace {
            return None;
        }
        self.next_token();

        let body = self.parse_block_statement()?;

        Some(Expression::Function {
            params,
            body: Box::new(body),
        })
    }

    fn parse_call_args(&mut self) -> Option<Vec<Expression>> {
        if self.next_token == Token::RParen {
            self.next_token();
            return None;
        }
        self.next_token();

        let mut args = Vec::new();
        args.push(self.parse_expression(Precedence::Lowest)?);

        while self.next_token == Token::Comma {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }

        if self.next_token != Token::RParen {
            return None;
        }
        self.next_token();

        Some(args)
    }

    fn parse_call_expression(&mut self, function: Expression) -> Option<Expression> {
        Some(Expression::Call {
            function: Box::new(function),
            args: self.parse_call_args(),
        })
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

            Token::LParen => {
                self.next_token();
                let expr = self.parse_expression(Precedence::Lowest)?;
                if self.next_token != Token::RParen {
                    self.peek_error(Token::RParen);
                    return None;
                }

                self.next_token();

                expr
            }

            Token::If => self.parse_if_expression()?,

            Token::Function => self.parse_fn_literal()?,

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

        Some(if op_token == Token::LParen {
            self.parse_call_expression(left)?
        } else {
            self.next_token();
            let right = self.parse_expression(precedence)?;
            Expression::Infix {
                left: Box::new(left),
                op: op_token,
                right: Box::new(right),
            }
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

        let expected_identifiers = [("x", 5), ("y", 10), ("foobar", 838383)];
        for (stmt, (expected_ident, expected_val)) in program.iter().zip(expected_identifiers) {
            if let Statement::Let {
                name: ident_token,
                value: val_token,
            } = stmt
            {
                let Token::Ident(name) = ident_token else {
                    panic!("expected identifier token found.");
                };

                let Expression::Int(val) = *val_token else {
                    panic!();
                };

                assert_eq!(name, expected_ident);
                assert_eq!(val, expected_val);
            }
        }
    }

    #[test]
    fn return_statements() {
        struct Return<'a> {
            input: &'a str,
            return_val: Option<Expression>,
        }

        let inputs = [
            Return {
                input: "return 5;",
                return_val: Some(Expression::Int(5)),
            },
            Return {
                input: "return 10;",
                return_val: Some(Expression::Int(10)),
            },
            Return {
                input: "return 993322;",
                return_val: Some(Expression::Int(993322)),
            },
            Return {
                input: "return;",
                return_val: None,
            },
        ];

        for input in inputs {
            let program = new_program(input.input).unwrap();
            if program.len() != 1 {
                panic!();
            }

            let Statement::Return(ref val) = program[0] else {
                panic!();
            };

            assert_eq!(input.return_val, *val);
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

    #[test]
    fn if_expression() {
        let input = "if (x < y) { x }";

        let program = new_program(input).unwrap();
        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(Expression::If {
            ref condition,
            ref consequence,
            ref alternative,
        }) = program[0]
        else {
            panic!();
        };

        let Expression::Infix {
            left: ref cond_left,
            op: ref cond_op,
            right: ref cond_right,
        } = **condition
        else {
            panic!();
        };

        let Expression::Ident(ref left) = **cond_left else {
            panic!()
        };
        let Expression::Ident(ref right) = **cond_right else {
            panic!()
        };

        let Statement::Block(ref consequence) = **consequence else {
            panic!();
        };

        let Statement::ExpressionStmt(Expression::Ident(ref consequence)) = consequence[0] else {
            panic!();
        };

        assert_eq!(left, "x");
        assert_eq!(right, "y");
        assert_eq!(*cond_op, Token::LT);
        assert_eq!(consequence, "x");
        assert!(alternative.is_none());
    }

    #[test]
    fn if_else_expression() {
        let input = r#"if (x < y) { x } else { y }"#;

        let program = new_program(input).unwrap();
        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(Expression::If {
            condition,
            consequence,
            alternative,
        }) = program.get(0).unwrap()
        else {
            panic!();
        };

        let Expression::Infix {
            left: ref cond_left,
            op: ref cond_op,
            right: ref cond_right,
        } = **condition
        else {
            panic!();
        };

        let Expression::Ident(ref left) = **cond_left else {
            panic!()
        };
        let Expression::Ident(ref right) = **cond_right else {
            panic!()
        };

        let Statement::Block(ref consequence) = **consequence else {
            panic!();
        };

        let Statement::ExpressionStmt(Expression::Ident(consequence)) = consequence.get(0).unwrap()
        else {
            panic!();
        };

        let alternative = alternative.as_ref().unwrap();
        let Statement::Block(ref alternative) = **alternative else {
            panic!();
        };

        let Statement::ExpressionStmt(Expression::Ident(ref alternative)) = alternative[0] else {
            panic!();
        };

        assert_eq!(left, "x");
        assert_eq!(right, "y");
        assert_eq!(*cond_op, Token::LT);
        assert_eq!(consequence, "x");
        assert_eq!(alternative, "y");
    }

    #[test]
    fn function_literal() {
        // ExpressionStmt(
        // Function {
        // params: Some(["x", "y"]),
        // body: Block([ExpressionStmt(Infix {
        // left: Ident("x"),
        // op: Plus,
        // right: Ident("y")
        // })])
        // })
        let program = new_program("fn(x, y) { x + y }").unwrap();

        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(Expression::Function {
            ref params,
            ref body,
        }) = program[0]
        else {
            panic!();
        };

        let params = params.clone().unwrap();
        let mut params = params.iter();
        let Statement::Block(ref body) = **body else {
            panic!();
        };

        let body = body.get(0).unwrap();
        println!("body: {:?}", body);

        assert_eq!(params.next().unwrap(), "x");
        assert_eq!(params.next().unwrap(), "y");
        assert_eq!(
            *body,
            Statement::ExpressionStmt(Expression::Infix {
                left: Box::new(Expression::Ident("x".into())),
                op: Token::Plus,
                right: Box::new(Expression::Ident("y".into()))
            })
        )
    }

    #[test]
    fn function_call() {
        let program = new_program("add(1, 2 * 3, 4 + 5);").unwrap();

        if program.len() != 1 {
            panic!();
        }

        let Statement::ExpressionStmt(Expression::Call {
            ref function,
            ref args,
        }) = program[0]
        else {
            panic!();
        };

        let Expression::Ident(ref function) = **function else {
            panic!();
        };

        let args = args.as_ref().unwrap();
        let mut args = args.iter();

        assert_eq!(function, "add");
        assert_eq!(args.next().unwrap(), &Expression::Int(1));

        // 2 * 3
        let Expression::Infix {
            left,
            op: Token::Asterisk,
            right,
        } = args.next().unwrap()
        else {
            panic!();
        };
        assert_eq!(**left, Expression::Int(2));
        assert_eq!(**right, Expression::Int(3));

        // 4 + 5
        let Expression::Infix {
            left,
            op: Token::Plus,
            right,
        } = args.next().unwrap()
        else {
            panic!();
        };
        assert_eq!(**left, Expression::Int(4));
        assert_eq!(**right, Expression::Int(5));
    }
}
