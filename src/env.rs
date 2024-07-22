use crate::interpreter::Object;
use std::collections::HashMap;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
}

impl<'a> Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn get(&self, name: &str) -> Option<&Object> {
        self.store.get(name)
    }

    pub fn set(&mut self, name: String, obj: Object) -> Object {
        self.store.entry(name).or_insert(obj).clone()
    }
}
