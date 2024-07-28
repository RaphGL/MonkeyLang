use crate::interpreter::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

pub type EnvCell = Rc<RefCell<Environment>>;

#[derive(PartialEq, Debug, Clone)]
pub struct Environment {
    store: HashMap<String, Object>,
    outer: Option<EnvCell>,
}

impl<'a> Environment {
    pub fn new() -> EnvCell {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: None,
        }))
    }

    pub fn new_enclosed_with(outer: EnvCell) -> EnvCell {
        Rc::new(RefCell::new(Self {
            store: HashMap::new(),
            outer: Some(Rc::clone(&outer)),
        }))
    }

    pub fn get(&self, name: &str) -> Option<Object> {
        let value = self.store.get(name);
        if let Some(value) = value {
            return Some(value.clone());
        }

        if let Some(ref outer) = self.outer {
            let value = outer.borrow().get(name)?;
            return Some(value);
        }

        None
    }

    pub fn set(&mut self, name: String, obj: Object) -> Object {
        self.store.insert(name, obj.clone());
        obj
    }
}
