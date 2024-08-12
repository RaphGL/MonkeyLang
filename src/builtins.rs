use crate::interpreter::Object;
use std::collections::HashMap;

pub type BuiltinFn = fn(&[Object]) -> Object;

pub struct Builtins<'a>(HashMap<&'a str, BuiltinFn>);

impl<'a> Builtins<'a> {
    pub fn new() -> Self {
        let builtins: HashMap<&str, BuiltinFn> = HashMap::from([
            ("len", len as BuiltinFn),
            ("first", first),
            ("last", last),
            ("rest", rest),
            ("push", push),
            ("puts", puts),
        ]);

        Self(builtins)
    }

    pub fn get(&self, builtin: &str) -> Option<BuiltinFn> {
        self.0.get(builtin).map(|f| *f)
    }
}

fn len(args: &[Object]) -> Object {
    if args.len() > 1 {
        return Object::Error(format!(
            "wrong number of argumnets for `len`. got {} expected {}",
            args.len(),
            1
        ));
    }

    let arg = args[0].clone();
    Object::Int(match arg {
        Object::Str(string) => string.len(),
        Object::Array(arr) => arr.len(),
        _ => return Object::Error(format!("len is not supported by `{}`", arg)),
    } as i64)
}

fn first(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got {} expected 1",
            args.len()
        ));
    }

    match args[0] {
        Object::Array(ref arr) => {
            if !arr.is_empty() {
                *arr[0].clone()
            } else {
                Object::Null
            }
        }

        ref arg @ _ => Object::Error(format!("argument to `first` must be an array, got {}", arg)),
    }
}

fn last(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got {} expected 1",
            args.len()
        ));
    }

    match args[0] {
        Object::Array(ref arr) => {
            if !arr.is_empty() {
                *arr[arr.len() - 1].clone()
            } else {
                Object::Null
            }
        }

        ref arg @ _ => Object::Error(format!("argument to `last` must be an array got {}", arg)),
    }
}

fn rest(args: &[Object]) -> Object {
    if args.len() != 1 {
        return Object::Error(format!(
            "wrong number of arguments. got {} expected 1",
            args.len()
        ));
    }

    match args[0] {
        Object::Array(ref array) => {
            if array.len() > 0 {
                Object::Array(array[1..].into())
            } else {
                Object::Null
            }
        }
        _ => Object::Null,
    }
}

fn push(args: &[Object]) -> Object {
    if args.len() != 2 {
        return Object::Error(format!(
            "wrong number of arguments. got {} expected 2",
            args.len()
        ));
    }

    let arr = match args[0] {
        Object::Array(ref arr) => arr,
        ref arg @ _ => {
            return Object::Error(format!("argument to `push` must be an array, got {arg}"))
        }
    };

    let mut new_arr = arr.clone();
    new_arr.push(Box::new(args[1].clone()));
    Object::Array(new_arr)
}

fn puts(args: &[Object]) -> Object {
    for arg in args {
        println!("{arg}");
    }

    Object::Null
}
