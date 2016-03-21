use ast::{Exp, ScopeStmt};
use std::collections::HashMap;
use std::ops::{Add, Mul, Sub, Div, Rem};

impl Exp {
    pub fn eval(&self, genv: &mut EnvObj, env: &mut EnvObj) -> Obj {
        // TODO(DarinM223): implement this
        Obj::Null
    }
}

impl ScopeStmt {
    pub fn exec(&self, genv: &mut EnvObj, env: &mut EnvObj) {
        // TODO(DarinM223): implement this
    }

    pub fn eval(&self, genv: &mut EnvObj, env: &mut EnvObj) -> Obj {
        // TODO(DarinM223): implement this
        Obj::Null
    }
}

pub fn interpret(stmt: ScopeStmt) {
    // TODO(DarinM223): implement this
}

/// An object used by the interpreter
#[derive(Clone)]
pub enum Obj {
    Null,
    Int(IntObj),
    Array(Box<ArrayObj>),
    Env(EnvObj),
}

impl Obj {
    pub fn obj_type(&self) -> i32 {
        match *self {
            Obj::Null => 0,
            Obj::Int(_) => 1,
            Obj::Array(_) => 2,
            Obj::Env(_) => 3,
        }
    }
}

#[derive(Clone, Eq, PartialEq, PartialOrd, Ord)]
pub struct IntObj {
    value: i32,
}

impl Add<IntObj> for IntObj {
    type Output = IntObj;

    fn add(self, other: IntObj) -> IntObj {
        IntObj { value: self.value + other.value }
    }
}

impl Sub<IntObj> for IntObj {
    type Output = IntObj;

    fn sub(self, other: IntObj) -> IntObj {
        IntObj { value: self.value - other.value }
    }
}

impl Mul<IntObj> for IntObj {
    type Output = IntObj;

    fn mul(self, other: IntObj) -> IntObj {
        IntObj { value: self.value * other.value }
    }
}

impl Div<IntObj> for IntObj {
    type Output = IntObj;

    fn div(self, other: IntObj) -> IntObj {
        IntObj { value: self.value / other.value }
    }
}

impl Rem<IntObj> for IntObj {
    type Output = IntObj;

    fn rem(self, other: IntObj) -> IntObj {
        IntObj { value: self.value % other.value }
    }
}

#[derive(Clone)]
pub struct ArrayObj {
    length: IntObj,
    arr: Vec<Obj>,
}

impl ArrayObj {
    pub fn new(length: IntObj, init: Obj) -> ArrayObj {
        let len = length.value;
        ArrayObj {
            length: length,
            arr: vec![init; len as usize],
        }
    }

    pub fn length(&self) -> IntObj {
        self.length.clone()
    }

    pub fn set(&mut self, i: IntObj, v: Obj) -> Obj {
        if let Some(mut item) = self.arr.get_mut(i.value as usize) {
            *item = v;
        }
        Obj::Null
    }

    pub fn get(&mut self, i: IntObj) -> Option<&mut Obj> {
        self.arr.get_mut(i.value as usize)
    }
}

/// Environment entries
#[derive(Clone)]
pub enum Entry {
    Var(Obj),
    Code,
}

#[derive(Clone)]
pub struct EnvObj {
    parent: Option<Box<EnvObj>>,
    table: HashMap<String, Entry>,
}

impl EnvObj {
    pub fn new(parent: Option<EnvObj>) -> EnvObj {
        EnvObj {
            parent: parent.map(|env| Box::new(env)),
            table: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: String, entry: Entry) {
        let mut curr_env = Some(self);

        loop {
            if let Some(ref mut curr_env) = curr_env {
                if let Some(_) = curr_env.table.get_mut(&name) {
                    break;
                }
            }

            if let Some(env) = curr_env.take() {
                if let Some(ref mut parent) = env.parent {
                    curr_env = Some(parent);
                } else {
                    break;
                }
            }
        }

        if let Some(curr_env) = curr_env {
            curr_env.table.insert(name, entry);
        }
    }

    pub fn get(&mut self, name: String) -> Option<&mut Entry> {
        let mut curr_env = Some(self);

        while let Some(env) = curr_env.take() {
            if let Some(data) = env.table.get_mut(&name) {
                return Some(data);
            }

            if let Some(ref mut parent) = env.parent {
                curr_env = Some(parent);
            } else {
                break;
            }
        }

        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_int_ord() {
        assert!(IntObj { value: 2 } > IntObj { value: 1 });
        assert!(IntObj { value: 1 } < IntObj { value: 2 });
        assert!(IntObj { value: -1 } < IntObj { value: 0 });
    }
}
