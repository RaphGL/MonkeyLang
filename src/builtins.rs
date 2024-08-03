use crate::interpreter::Object;
use std::collections::HashMap;

pub type BuiltinFn = fn(&[Object]) -> Object;

pub struct Builtins<'a>(HashMap<&'a str, BuiltinFn>);

impl<'a> Builtins<'a> {
    pub fn new() -> Self {
        let mut builtins: HashMap<&str, BuiltinFn> = HashMap::new();
        builtins.insert("len", len);
        builtins.insert("first", first);
        builtins.insert("last", last);

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
    let Object::Str(string) = arg else {
        return Object::Error(format!("len is not supported by `{}`", arg));
    };

    Object::Int(string.len() as i64)
}

fn first(args: &[Object]) -> Object {
    Object::Null
}

fn last(args: &[Object]) -> Object {
    Object::Null
}
