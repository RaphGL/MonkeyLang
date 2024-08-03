use std::fmt::Display;
use std::rc::Rc;
use std::sync::LazyLock;

use crate::builtins::{BuiltinFn, Builtins};
use crate::env::{EnvCell, Environment};
use crate::lexer::Token;
use crate::parser::{Expression, Program, Statement};

static BUILTINS: LazyLock<Builtins> = LazyLock::new(|| Builtins::new());

#[derive(Debug, PartialEq, Clone)]
pub enum Object {
    Int(i64),
    Bool(bool),
    Str(String),
    Return(Box<Object>),
    Error(String),
    Builtin(String, BuiltinFn),

    FunctionLiteral {
        params: Option<Vec<String>>,
        body: Statement,
        env: EnvCell,
    },
    Null,
}

impl Display for Object {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(int) => write!(f, "{int}"),
            Self::Bool(boolean) => write!(f, "{boolean}"),
            Self::Str(string) => write!(f, "\"{string}\""),
            Self::Null => write!(f, "null"),
            Self::Builtin(fn_name, _) => write!(f, "builtin: {fn_name}"),
            Self::Return(ret) => ret.fmt(f),
            Self::Error(err) => write!(f, "error: {err}"),

            Self::FunctionLiteral { params, .. } => {
                let mut params_str = String::new();
                if let Some(params) = params {
                    for (i, param) in params.iter().enumerate() {
                        params_str += param;
                        if i < params.len() - 1 {
                            params_str += ", ";
                        }
                    }
                }

                // todo: add body rendering
                write!(f, "fn({params_str}) {{ <body> }}")
            }
        }
    }
}

pub fn eval(program: &Program, env: EnvCell) -> Object {
    let mut result = Object::Null;

    for stmt in program {
        result = eval_statement(&stmt, Rc::clone(&env));
        if let Object::Return(obj) = result {
            return *obj;
        }

        if let Object::Error(_) = result {
            return result;
        }
    }

    result
}

pub fn eval_statement(stmt: &Statement, env: EnvCell) -> Object {
    match stmt {
        Statement::Let { name, value } => {
            let value = eval_expression(value, Rc::clone(&env));
            if let Object::Error(_) = value {
                return value;
            }

            let Token::Ident(name) = name else {
                return Object::Error(format!("invalid syntax: {name} used instead of identifier"));
            };

            env.borrow_mut().set(name.clone(), value)
        }

        Statement::Return(return_val) => {
            if let Some(return_val) = return_val {
                let return_val = eval_expression(return_val, env);
                Object::Return(Box::new(return_val))
            } else {
                Object::Null
            }
        }

        Statement::Block(block) => {
            let mut result = Object::Null;

            for stmt in block {
                result = eval_statement(&stmt, Rc::clone(&env));
                match result {
                    Object::Return(_) | Object::Error(_) => return result,
                    _ => (),
                }
            }

            result
        }

        Statement::ExpressionStmt(expr) => eval_expression(expr, env),
    }
}

fn is_truthy(obj: Object) -> bool {
    match obj {
        Object::Null | Object::Bool(false) => false,
        _ => true,
    }
}

fn apply_function(func: Object, args: Option<Vec<Object>>) -> Object {
    let Object::FunctionLiteral { params, body, env } = func else {
        if let Object::Builtin(_, builtin) = func {
            return if let Some(args) = args {
                builtin(&args)
            } else {
                builtin(&[])
            };
        }

        return Object::Error("not a function".into());
    };

    let local_env = {
        let env = Environment::new_enclosed_with(Rc::clone(&env));

        if let Some(args) = args {
            let params = params.expect("params should be the same size as args");
            for (i, param) in params.iter().enumerate() {
                env.borrow_mut().set(param.clone(), args[i].clone());
            }
        }

        env
    };

    let result = eval_statement(&body, local_env);

    if let Object::Return(result) = result {
        *result
    } else {
        result
    }
}

fn eval_expression(expr: &Expression, env: EnvCell) -> Object {
    match expr {
        Expression::Int(int) => Object::Int(*int),

        Expression::Str(string) => Object::Str(string.clone()),

        Expression::Ident(ident) => {
            if ident == "null" {
                return Object::Null;
            }

            if let Some(value) = env.borrow().get(ident) {
                return value.clone();
            }

            if let Some(func) = BUILTINS.get(ident) {
                return Object::Builtin(ident.clone(), func);
            };

            Object::Error(format!("identifier not found: {ident}"))
        }

        Expression::PrefixNeg(right) => {
            let result = eval_expression(right, env);
            if let Object::Int(int) = result {
                return Object::Int(-int);
            }

            Object::Error(format!("type error: expected int found {result}"))
        }

        Expression::PrefixNot(right) => {
            let result = eval_expression(right, env);
            match result {
                Object::Bool(bool) => Object::Bool(!bool),
                Object::Null => Object::Bool(true),
                _ => Object::Bool(false),
            }
        }

        Expression::Boolean(boolean) => Object::Bool(*boolean),

        Expression::Infix { left, op, right } => {
            let left = eval_expression(left, Rc::clone(&env));
            let right = eval_expression(right, Rc::clone(&env));

            match left {
                Object::Bool(left) => {
                    let Object::Bool(right) = right else {
                        return Object::Error(format!(
                            "type mismatch: expected bool on right-hand side found {right}"
                        ));
                    };

                    match *op {
                        Token::Eq => Object::Bool(left == right),
                        Token::NotEq => Object::Bool(left != right),
                        _ => Object::Error(format!(
                            "unknown operator: operator `{op}` does not work on bools"
                        )),
                    }
                }

                Object::Int(left) => {
                    let Object::Int(right) = right else {
                        return Object::Error(format!(
                            "type mismatch: expected int on right-hand side found {right}"
                        ));
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
                        _ => Object::Error(format!("unknown operator: {op}")),
                    }
                }

                Object::Str(left) => {
                    if *op != Token::Plus {
                        return Object::Error(format!("invalid string operator {}", *op));
                    }
                    let Object::Str(right) = right else {
                        return Object::Error(format!(
                            "type mismatch: expected string on right-hand side found {right}"
                        ));
                    };

                    Object::Str(left + &right)
                }

                _ => Object::Error(format!(
                    "type error: infix operator `{op}` does not work on {left}"
                )),
            }
        }

        Expression::If {
            condition,
            consequence,
            alternative,
        } => {
            let condition = eval_expression(condition, Rc::clone(&env));
            if is_truthy(condition) {
                eval_statement(consequence, Rc::clone(&env))
            } else if let Some(alternative) = alternative {
                eval_statement(alternative, Rc::clone(&env))
            } else {
                Object::Null
            }
        }

        Expression::Function { params, body } => Object::FunctionLiteral {
            params: params.clone(),
            body: *body.clone(),
            env: Rc::clone(&env),
        },

        Expression::Call { function, args } => {
            let func = eval_expression(function, Rc::clone(&env));
            if let Object::Error(_) = func {
                return func;
            }

            let args = if let Some(args) = args {
                let mut eval_args = Vec::new();
                for arg in args {
                    eval_args.push(eval_expression(arg, Rc::clone(&env)));
                }

                Some(eval_args)
            } else {
                None
            };

            return apply_function(func, args);
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::env::Environment;
    use crate::lexer::Lexer;
    use crate::parser::{Expression, Parser, Program, Statement};

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

        let env = Environment::new();
        assert_eq!(eval(&program, env), Object::Int(532));
    }

    #[test]
    fn int_literal() {
        let program = new_program("true").unwrap();
        if program.len() != 1 {
            panic!();
        }

        let env = Environment::new();
        assert_eq!(eval(&program, env), Object::Bool(true));
    }

    #[test]
    fn null_literal() {
        let program = new_program("null").unwrap();
        let env = Environment::new();
        assert_eq!(eval(&program, env), Object::Null);
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
            let env = Environment::new();
            assert_eq!(eval(&program, env), expected);
        }
    }

    #[test]
    fn neg_operator() {
        let tests = [("-50", Object::Int(-50)), ("--50", Object::Int(50))];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            let env = Environment::new();
            assert_eq!(eval(&program, env), expected);
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
            let env = Environment::new();
            assert_eq!(eval(&program, env), expected);
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
            let env = Environment::new();
            assert_eq!(eval(&program, env), expected);
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
            let env = Environment::new();
            assert_eq!(eval(&program, env), expected);
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
            let env = Environment::new();
            let ret = eval(&program, env);

            assert_eq!(ret, expected);
        }
    }

    #[test]
    fn let_statements() {
        let tests = [
            ("let a = 5; a;", Object::Int(5)),
            ("let a = 5 * 5; a;", Object::Int(25)),
            ("let a = 5; let b = a; b;", Object::Int(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Object::Int(15),
            ),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            let env = Environment::new();
            let ret = eval(&program, env);

            assert_eq!(ret, expected);
        }
    }

    #[test]
    fn error_handling() {
        let tests = [
            "5 + true;",
            "5 + true; 5;",
            "-true",
            "true + false;",
            "5; true + false; 5",
            "if (10 > 1) { true + false; }",
            "5 + true;",
            "5 + true; 5;",
            "-true",
            "true + false;",
            "5; true + false; 5",
            "if (10 > 1) { true + false; }",
            "foobar",
            "\"Hello\" - \"World!\"",
        ];

        for input in tests {
            let program = new_program(input).unwrap();
            let env = Environment::new();
            let ret = eval(&program, env);

            let Object::Error(_) = ret else {
                panic!();
            };
        }
    }

    #[test]
    fn function_object() {
        let input = "fn(x) { x + 2; };";

        let program = new_program(input).unwrap();
        let Object::FunctionLiteral { params, body, .. } = eval(&program, Environment::new())
        else {
            panic!();
        };

        let Some(params) = params else {
            panic!();
        };

        let Statement::Block(body) = body else {
            panic!();
        };

        assert_eq!(params[0], "x");
        assert_eq!(
            body[0],
            Statement::ExpressionStmt(Expression::Infix {
                left: Box::new(Expression::Ident("x".into())),
                op: crate::lexer::Token::Plus,
                right: Box::new(Expression::Int(2))
            })
        );
    }

    #[test]
    fn string_object() {
        let program = new_program("\"hello world\"").unwrap();
        assert!(matches!(eval(&program, Environment::new()), Object::Str(_)));
    }

    #[test]
    fn function_application() {
        let tests = [
            ("let identity = fn(x) { x; }; identity(5);", Object::Int(5)),
            (
                "let identity = fn(x) { return x; }; identity(5);",
                Object::Int(5),
            ),
            ("let double = fn(x) { x * 2; }; double(5);", Object::Int(10)),
            ("let add = fn(x, y) { x + y; }; add(5, 5);", Object::Int(10)),
            (
                "let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));",
                Object::Int(20),
            ),
            ("fn(x) { x; }(5)", Object::Int(5)),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            let env = Environment::new();
            let ret = eval(&program, env);

            assert_eq!(ret, expected);
        }
    }

    #[test]
    fn closures() {
        let input = "
            let newAdder = fn(x) {
                fn(y) { x + y };
            };

            let addTwo = newAdder(2);
            addTwo(2);
        ";

        let program = new_program(input).unwrap();
        let env = Environment::new();
        assert_eq!(eval(&program, env), Object::Int(4));
    }

    #[test]
    fn string_concatenation() {
        let input = "
            \"Hello\" + \" \" + \"World!\"
        ";

        let program = new_program(input).unwrap();
        let env = Environment::new();
        assert_eq!(eval(&program, env), Object::Str("Hello World!".into()));
    }

    #[test]
    fn builtin_functions() {
        let tests = [
            ("len(\"\")", Object::Int(0)),
            ("len(\"four\")", Object::Int(4)),
            ("len(\"hello world\")", Object::Int(11)),
            ("len(1)", Object::Error("".into())),
            ("len(\"one\", \"two\")", Object::Error("".into())),
        ];

        for (input, expected) in tests {
            let program = new_program(input).unwrap();
            let env = Environment::new();
            let ret = eval(&program, env);

            if matches!(expected, Object::Error(_)) {
                assert!(matches!(ret, Object::Error(_)));
            } else {
                assert!(ret == expected);
            }
        }
    }
}
