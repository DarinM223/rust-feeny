use ast::{Exp, ScopeStmt};
use std::collections::HashMap;
use std::ops::{Add, Mul, Sub, Div, Rem};

impl Exp {
    pub fn eval(&self, genv: &mut EnvObj, env: &mut EnvObj) -> Obj {
        // TODO(DarinM223): implement this
        match *self {
            Exp::Int(i) => Obj::Int(IntObj { value: i }),
            Exp::Null => Obj::Null,
            Exp::Printf(ref printf) => {
                // TODO(DarinM223): implement this
                Obj::Null
            }
            Exp::Array(ref array) => {
                let length = array.length.eval(genv, env);
                let init = array.init.eval(genv, env);
                Obj::Array(Box::new(ArrayObj::new(length, init)))
            }
            Exp::Object(ref obj) => {
                let env_obj = EnvObj::new(Some(obj.parent.eval(genv, env)));
                Obj::Env(env_obj)
            }
            Exp::Slot(ref slot) => {
                let obj = slot.exp.eval(genv, env);
                if let Obj::Env(mut env_obj) = obj {
                    let entry = env_obj.get(&slot.name[..]);
                    if let Some(&mut Entry::Var(ref obj)) = entry {
                        return obj.clone();
                    } else {
                        panic!("The object should contain a Var");
                        unreachable!();
                    }
                } else {
                    panic!("Object has to be an environment object");
                    unreachable!();
                }
            }
            Exp::SetSlot(ref setslot) => {
                let obj = setslot.exp.eval(genv, env);
                let value = setslot.value.eval(genv, env);
                if let Obj::Env(mut env_obj) = obj {
                    env_obj.add(&setslot.name[..], Entry::Var(value));
                } else {
                    panic!("Object has to be an environment object");
                    unreachable!();
                }

                Obj::Null
            }
            Exp::CallSlot(ref callslot) => {
                let obj = callslot.exp.eval(genv, env);
                // TODO(DarinM223): implement this
                match obj {
                    Obj::Int(ref iexp) => Obj::Null,
                    Obj::Array(ref arr) => Obj::Null,
                    Obj::Env(ref env) => Obj::Null,
                    _ => unreachable!(),
                }
            }
            // TODO(DarinM223): implement rest of this
            Exp::Call(ref call) => Obj::Null,
            Exp::Set(ref set) => Obj::Null,
            Exp::If(ref iexp) => Obj::Null,
            Exp::While(ref wexp) => Obj::Null,
            Exp::Ref(ref name) => Obj::Null,
        }
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
    pub fn new(length: Obj, init: Obj) -> ArrayObj {
        let len = if let Obj::Int(i) = length {
            i.value
        } else {
            unreachable!();
        };

        ArrayObj {
            length: IntObj { value: len },
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
    Func(ScopeStmt, Vec<String>),
}

#[derive(Clone)]
pub struct EnvObj {
    parent: Option<Box<EnvObj>>,
    table: HashMap<String, Entry>,
}

impl EnvObj {
    pub fn new(parent: Option<Obj>) -> EnvObj {
        EnvObj {
            parent: parent.map(|env| {
                if let Obj::Env(env) = env {
                    Box::new(env)
                } else {
                    unreachable!()
                }
            }),
            table: HashMap::new(),
        }
    }

    pub fn add(&mut self, name: &str, entry: Entry) {
        let mut curr_env = Some(self);

        while let Some(env) = curr_env.take() {
            if let Some(_) = env.table.get_mut(name) {
                break;
            }

            if let Some(ref mut parent) = env.parent {
                curr_env = Some(parent);
            } else {
                break;
            }
        }

        if let Some(curr_env) = curr_env {
            curr_env.table.insert(name.to_owned(), entry);
        }
    }

    pub fn get(&mut self, name: &str) -> Option<&mut Entry> {
        let mut curr_env = Some(self);

        while let Some(env) = curr_env.take() {
            if let Some(data) = env.table.get_mut(name) {
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
