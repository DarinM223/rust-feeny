use ast::{Exp, ScopeStmt};
use std::collections::HashMap;
use std::io;
use std::ops::{Add, Mul, Sub, Div, Rem};

fn get_entry<'a>(name: &str, genv: &'a mut EnvObj, env: &'a mut Obj) -> &'a mut Entry {
    let mut ent = None;
    if let &mut Obj::Env(ref mut env) = env {
        ent = env.get(name);
    }
    if let None = ent {
        ent = genv.get(name);
    }

    ent.unwrap()
}

type Error = io::Error;

impl Exp {
    pub fn eval(&self, genv: &mut EnvObj, env: &mut Obj) -> io::Result<Obj> {
        use std::io::ErrorKind::*;

        match *self {
            Exp::Int(i) => Ok(Obj::Int(IntObj { value: i })),
            Exp::Null => Ok(Obj::Null),
            Exp::Printf(ref printf) => {
                // TODO(DarinM223): implement this
                Ok(Obj::Null)
            }
            Exp::Array(ref array) => {
                let length = try!(array.length.eval(genv, env));
                let init = try!(array.init.eval(genv, env));
                Ok(Obj::Array(Box::new(ArrayObj::new(length, init))))
            }
            Exp::Object(ref obj) => {
                let env_obj = EnvObj::new(Some(try!(obj.parent.eval(genv, env))));
                Ok(Obj::Env(env_obj))
            }
            Exp::Slot(ref slot) => {
                let obj = try!(slot.exp.eval(genv, env));
                if let Obj::Env(mut env_obj) = obj {
                    let entry = env_obj.get(&slot.name[..]);
                    if let Some(&mut Entry::Var(ref obj)) = entry {
                        Ok(obj.clone())
                    } else {
                        Err(Error::new(InvalidData, "The object should contain a Var"))
                    }
                } else {
                    Err(Error::new(InvalidData, "Object has to be an environment type"))
                }
            }
            Exp::SetSlot(ref setslot) => {
                let obj = try!(setslot.exp.eval(genv, env));
                let value = try!(setslot.value.eval(genv, env));
                if let Obj::Env(mut env_obj) = obj {
                    env_obj.add(&setslot.name[..], Entry::Var(value));
                } else {
                    return Err(Error::new(InvalidData, "Object has to be an environment object"));
                }

                Ok(Obj::Null)
            }
            Exp::CallSlot(ref cs) => {
                let mut obj = try!(cs.exp.eval(genv, env));
                match obj {
                    Obj::Int(iexp) => {
                        let other = match try!(cs.args[0].eval(genv, env)) {
                            Obj::Int(i) => i,
                            _ => panic!("Operand has to be an integer"),
                        };

                        Ok(Obj::Int(match &cs.name[..] {
                            "add" => iexp + other,
                            "sub" => iexp - other,
                            "mul" => iexp * other,
                            "div" => iexp / other,
                            "mod" => iexp % other,
                            "lt" => IntObj::from_bool(iexp < other),
                            "gt" => IntObj::from_bool(iexp > other),
                            "le" => IntObj::from_bool(iexp <= other),
                            "ge" => IntObj::from_bool(iexp >= other),
                            "eq" => IntObj::from_bool(iexp == other),
                            _ => panic!("Invalid slot"),
                        }))
                    }
                    Obj::Array(ref mut arr) => {
                        let (ge, e) = (genv, env);
                        match &cs.name[..] {
                            "length" => Ok(Obj::Int(arr.length())),
                            "set" => {
                                Ok(arr.set(try!(cs.args[0].eval(ge, e)),
                                           try!(cs.args[1].eval(ge, e))))
                            }
                            "get" => {
                                Ok(arr.get(try!(cs.args[0].eval(ge, e)))
                                      .map(|obj| obj.clone())
                                      .unwrap_or(Obj::Null))
                            }
                            _ => Err(Error::new(InvalidInput, "Invalid slot")),
                        }
                    }
                    Obj::Env(ref mut ent) => {
                        let ent_clone = ent.clone();
                        if let Some(&mut Entry::Func(ref fun, ref args)) = ent.get(&cs.name[..]) {
                            if cs.nargs as usize != args.len() {
                                return Err(Error::new(InvalidInput, "Args number doesn't match"));
                            }

                            let mut new_env = EnvObj::new(None);
                            for (i, arg) in cs.args.iter().enumerate() {
                                new_env.add(&args[i][..], Entry::Var(try!(arg.eval(genv, env))));
                            }
                            // FIXME(DarinM223): cloning might not work because if the this object
                            // gets modified it won't change the original object
                            new_env.add("this", Entry::Var(Obj::Env(ent_clone)));

                            fun.eval(genv, &mut Obj::Env(new_env))
                        } else {
                            Err(Error::new(InvalidInput, "Function is not found"))
                        }
                    }
                    _ => unreachable!(),
                }
            }
            Exp::Call(ref call) => {
                let (fun, args) = match genv.get(&call.name[..]) {
                    Some(&mut Entry::Func(ref fun, ref args)) => (fun.clone(), args.clone()),
                    _ => return Err(Error::new(InvalidInput, "Function not found")),
                };

                if call.nargs as usize != args.len() {
                    return Err(Error::new(InvalidData, "Args number does not match"));
                }

                let mut new_env = EnvObj::new(None);
                for (i, arg) in call.args.iter().enumerate() {
                    new_env.add(&args[i][..], Entry::Var(try!(arg.eval(genv, env))));
                }

                fun.eval(genv, &mut Obj::Env(new_env))
            }
            Exp::Set(ref set) => {
                let res = try!(set.exp.eval(genv, env));
                let ent = get_entry(&set.name[..], genv, env);

                match *ent {
                    Entry::Var(_) => *ent = Entry::Var(res),
                    Entry::Func(_, _) => return Err(Error::new(InvalidData, "Setting func")),
                };
                Ok(Obj::Null)
            }
            Exp::If(ref iexp) => {
                let pred = try!(iexp.pred.eval(genv, env));
                match pred {
                    Obj::Null => iexp.alt.eval(genv, env),
                    _ => iexp.conseq.eval(genv, env),
                }
            }
            Exp::While(ref wexp) => {
                while let Obj::Int(_) = try!(wexp.pred.eval(genv, env)) {
                    try!(wexp.body.eval(genv, env));
                }
                Ok(Obj::Null)
            }
            Exp::Ref(ref name) => {
                let ent = get_entry(name, genv, env);

                match *ent {
                    Entry::Var(ref obj) => Ok(obj.clone()),
                    Entry::Func(_, _) => Err(Error::new(InvalidInput, "ref to function")),
                }
            }
        }
    }
}

impl ScopeStmt {
    pub fn exec(&self, genv: &mut EnvObj, env: &mut Obj) {
        // TODO(DarinM223): implement this
    }

    pub fn eval(&self, genv: &mut EnvObj, env: &mut Obj) -> io::Result<Obj> {
        // TODO(DarinM223): implement this
        Ok(Obj::Null)
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

impl IntObj {
    pub fn from_bool(b: bool) -> IntObj {
        match b {
            true => IntObj { value: 1 },
            false => IntObj { value: 0 },
        }
    }
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
        let len = match length {
            Obj::Int(i) => i.value,
            _ => unreachable!(),
        };

        ArrayObj {
            length: IntObj { value: len },
            arr: vec![init; len as usize],
        }
    }

    pub fn length(&self) -> IntObj {
        self.length.clone()
    }

    pub fn set(&mut self, i: Obj, v: Obj) -> Obj {
        let index = match i {
            Obj::Int(i) => i.value,
            _ => unreachable!(),
        };

        if let Some(mut item) = self.arr.get_mut(index as usize) {
            *item = v;
        }
        Obj::Null
    }

    pub fn get(&mut self, i: Obj) -> Option<&mut Obj> {
        let index = match i {
            Obj::Int(i) => i.value,
            _ => unreachable!(),
        };
        self.arr.get_mut(index as usize)
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
