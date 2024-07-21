use std::fmt::Display;

use crate::lexer::Token;
use crate::parser::{Expression, Program, Statement};

#[derive(Debug, PartialEq, Eq)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Return(Box<Object>),
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Bool(boolean) => write!(f, "{boolean}"),
            Self::Null => write!(f, "null"),
            Self::Return(ret) => ret.fmt(f),
        }
    }
}

pub fn eval(program: &Program) -> Object {
    let mut result = Object::Null;

    for stmt in program {
        result = eval_statement(&stmt);
        if let Object::Return(obj) = result {
            return *obj;
        }
    }

    result
}

pub fn eval_statement(stmt: &Statement) -> Object {
    match stmt {
        Statement::Let { name, value } => todo!(),

        Statement::Return(return_val) => {
            if let Some(return_val) = return_val {
                let return_val = eval_expression(return_val);
                Object::Return(Box::new(return_val))
            } else {
                Object::Null
            }
        }

        Statement::Block(block) => {
            let mut result = Object::Null;

            for stmt in block {
                result = eval_statement(&stmt);
                if let Object::Return(_) = result {
                    return result;
                }
            }

            result
        }

        Statement::ExpressionStmt(expr) => eval_expression(expr),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null | Object::Bool(false) => false,
        _ => true,
    }
}

fn eval_expression(expr: &Expression) -> Object {
    match expr {
        Expression::Int(int) => Object::Int(*int),

        Expression::Ident(ident) => {
            if ident == "null" {
                Object::Null
            } else {
                todo!()
            }
        }

        Expression::PrefixNeg(right) => {
            let result = eval_expression(right);
            if let Object::Int(int) = result {
                return Object::Int(-int);
            }

            Object::Null
        }

        Expression::PrefixNot(right) => {
            let result = eval_expression(right);
            match result {
                Object::Bool(bool) => Object::Bool(!bool),
                Object::Null => Object::Bool(true),
                _ => Object::Bool(false),
            }
        }

        Expression::Boolean(boolean) => Object::Bool(*boolean),

        Expression::Infix { left, op, right } => {
            let left = eval_expression(left);
            let right = eval_expression(right);

            match left {
                Object::Bool(left) => {
                    let Object::Bool(right) = right else {
                        return Object::Null;
                    };

                    match *op {
                        Token::Eq => Object::Bool(left == right),
                        Token::NotEq => Object::Bool(left != right),
                        _ => Object::Null,
                    }
                }

                Object::Int(left) => {
                    let Object::Int(right) = right else {
                        return Object::Null;
                    };

                    match *op {
                        Token::Plus => Object::Int(left + right),
                        Token::Minus => Object::Int(left - right),
                        Token::Asterisk => Object::Int(left * right),
                        Token::Slash => Object::Int(left / right),
                        Token::GT => Object::Bool(left > right),
                        Token::LT => Object::Bool(left < right),
                        Token::Eq => Object::Bool(left == right),
                        Token::NotEq => Object::Bool(left != right),
                        _ => Object::Null,
                    }
                }

                _ => Object::Null,
            }
        }

        Expression::If {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval_expression(condition);
            if is_truthy(condition) {
                eval_statement(consequence)
            } else if let Some(alternative) = alternative {
                eval_statement(alternative)
            } else {
                Object::Null
            }
        }

        Expression::Function { params, body } => todo!(),
        Expression::Call { function, args } => todo!(),
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::{Parser, Program};

    use super::{eval, Object};

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
    fn bool_literal() {
        let program = new_program("532").unwrap();
        if program.len() != 1 {
            panic!();
        }

        assert_eq!(eval(&program), Object::Int(532));
    }

    #[test]
    fn int_literal() {
        let program = new_program("true").unwrap();
        if program.len() != 1 {
            panic!();
        }

        assert_eq!(eval(&program), Object::Bool(true));
    }

    #[test]
    fn null_literal() {
        let program = new_program("null").unwrap();
        assert_eq!(eval(&program), Object::Null);
    }

    #[test]
    fn bang_operator() {
        let tests = [
            ("!false", Object::Bool(true)),
            ("!true", Object::Bool(false)),
            ("!null", Object::Bool(true)),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            assert_eq!(eval(&program), expected);
        }
    }

    #[test]
    fn neg_operator() {
        let tests = [("-50", Object::Int(-50)), ("--50", Object::Int(50))];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            assert_eq!(eval(&program), expected);
        }
    }

    #[test]
    fn eval_integer_expression() {
        let tests = [
            ("5", Object::Int(5)),
            ("10", Object::Int(10)),
            ("-5", Object::Int(-5)),
            ("-10", Object::Int(-10)),
            ("5 + 5 + 5 + 5 - 10", Object::Int(10)),
            ("2 * 2 * 2 * 2 * 2", Object::Int(32)),
            ("-50 + 100 + -50", Object::Int(0)),
            ("5 * 2 + 10", Object::Int(20)),
            ("5 + 2 * 10", Object::Int(25)),
            ("20 + 2 * -10", Object::Int(0)),
            ("50 / 2 * 2 + 10", Object::Int(60)),
            ("2 * (5 + 10)", Object::Int(30)),
            ("3 * 3 * 3 + 10", Object::Int(37)),
            ("3 * (3 * 3) + 10", Object::Int(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Object::Int(50)),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            assert_eq!(eval(&program), expected);
        }
    }

    #[test]
    fn eval_boolean_expression() {
        let tests = [
            ("true", Object::Bool(true)),
            ("false", Object::Bool(false)),
            ("1 < 2", Object::Bool(true)),
            ("1 > 2", Object::Bool(false)),
            ("1 < 1", Object::Bool(false)),
            ("1 > 1", Object::Bool(false)),
            ("1 == 1", Object::Bool(true)),
            ("1 != 1", Object::Bool(false)),
            ("1 == 2", Object::Bool(false)),
            ("1 != 2", Object::Bool(true)),
            ("true == true", Object::Bool(true)),
            ("false == false", Object::Bool(true)),
            ("true == false", Object::Bool(false)),
            ("true != false", Object::Bool(true)),
            ("false != true", Object::Bool(true)),
            ("(1 < 2) == true", Object::Bool(true)),
            ("(1 < 2) == false", Object::Bool(false)),
            ("(1 > 2) == true", Object::Bool(false)),
            ("(1 > 2) == false", Object::Bool(true)),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            assert_eq!(eval(&program), expected);
        }
    }

    #[test]
    fn if_else_expressions() {
        let tests = [
            ("if (true) { 10 }", Object::Int(10)),
            ("if (false) { 10 }", Object::Null),
            ("if (1) { 10 }", Object::Int(10)),
            ("if (1 < 2) { 10 }", Object::Int(10)),
            ("if (1 > 2) { 10 }", Object::Null),
            ("if (1 > 2) { 10 } else { 20 }", Object::Int(20)),
            ("if (1 < 2) { 10 } else { 20 }", Object::Int(10)),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            assert_eq!(eval(&program), expected);
        }
    }

    #[test]
    fn return_statements() {
        let tests = [
            ("return 10;", Object::Int(10)),
            ("return 10; 9;", Object::Int(10)),
            ("return 2 * 5; 9;", Object::Int(10)),
            ("9; return 2 * 5; 9;", Object::Int(10)),
            (
                "if (10 > 1) {
                if (10 > 1) {
                    return 10;
                }
                 return 1;
            }",
                Object::Int(10),
            ),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            let ret = eval(&program);

            assert_eq!(ret, expected);
        }
    }
}
