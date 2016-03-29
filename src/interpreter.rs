use ast::{Exp, ScopeStmt, SlotStmt};
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
                debug!("Printing!");
                let mut counter = 0;
                for ch in printf.format.chars() {
                    if ch == '~' {
                        let res = try!(printf.exps[counter].eval(genv, env));
                        if let Obj::Int(obj) = res {
                            print!("{}", obj.value);
                        } else {
                            return Err(Error::new(InvalidInput, "Can only print Ints"));
                        }
                        counter += 1;
                    } else {
                        print!("{}", ch);
                    }
                }
                Ok(Obj::Null)
            }
            Exp::Array(ref array) => {
                let length = try!(array.length.eval(genv, env));
                let init = try!(array.init.eval(genv, env));
                Ok(Obj::Array(Box::new(ArrayObj::new(length, init))))
            }
            Exp::Object(ref obj) => {
                debug!("Object!");
                let mut env_obj = Obj::Env(EnvObj::new(Some(try!(obj.parent.eval(genv, env)))));
                for slot in &obj.slots {
                    slot.exec(genv, env, &mut env_obj);
                }
                Ok(env_obj)
            }
            Exp::Slot(ref slot) => {
                debug!("Getting slot!");
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
                debug!("Setting slot!");
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
                debug!("Calling slot: {}", &cs.name[..]);
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
                        debug!("Entry: {:?}", ent);
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
                debug!("Calling function: {}", &call.name[..]);
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
                debug!("New environment: {:?}", new_env);

                fun.eval(genv, &mut Obj::Env(new_env))
            }
            Exp::Set(ref set) => {
                debug!("Setting!");
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

impl SlotStmt {
    pub fn exec(&self, genv: &mut EnvObj, env: &mut Obj, obj: &mut Obj) -> io::Result<()> {
        use std::io::ErrorKind::*;

        if let Obj::Env(ref mut env_obj) = *obj {
            match *self {
                SlotStmt::Var(ref var) => {
                    env_obj.add(&var.name[..], Entry::Var(try!(var.exp.eval(genv, env))));
                }
                SlotStmt::Method(ref met) => {
                    env_obj.add(&met.name[..],
                                Entry::Func(met.body.clone(), met.args.clone()));
                }
            }

            Ok(())
        } else {
            Err(Error::new(InvalidData, "Expected object to be an environment object"))
        }
    }
}

impl ScopeStmt {
    pub fn eval(&self, genv: &mut EnvObj, env: &mut Obj) -> io::Result<Obj> {
        match *self {
            ScopeStmt::Var(ref var) => {
                debug!("Var!");
                let entry_obj = try!(var.exp.eval(genv, env));
                if let Obj::Env(ref mut env_obj) = *env {
                    env_obj.add(&var.name[..], Entry::Var(entry_obj));
                } else {
                    genv.add(&var.name[..], Entry::Var(entry_obj));
                }
                Ok(Obj::Null)
            }
            ScopeStmt::Fn(ref fun) => {
                debug!("Function: {}", &fun.name[..]);
                genv.add(&fun.name[..],
                         Entry::Func(fun.body.clone(), fun.args.clone()));
                Ok(Obj::Null)
            }
            ScopeStmt::Seq(ref seq) => {
                debug!("Sequence!");
                try!(seq.a.eval(genv, env));
                Ok(try!(seq.b.eval(genv, env)))
            }
            ScopeStmt::Exp(ref exp) => Ok(try!(exp.eval(genv, env))),
        }
    }
}

pub fn interpret(stmt: ScopeStmt) -> io::Result<()> {
    try!(stmt.eval(&mut EnvObj::new(None), &mut Obj::Null));
    Ok(())
}

/// An object used by the interpreter
#[derive(Clone, Debug)]
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

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
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

#[derive(Clone, Debug)]
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
#[derive(Clone, Debug)]
pub enum Entry {
    Var(Obj),
    Func(ScopeStmt, Vec<String>),
}

#[derive(Clone, Debug)]
pub struct EnvObj {
    parent: Option<Box<EnvObj>>,
    table: HashMap<String, Entry>,
}

impl EnvObj {
    pub fn new(parent: Option<Obj>) -> EnvObj {
        EnvObj {
            parent: parent.and_then(|env| {
                if let Obj::Env(env) = env {
                    Some(Box::new(env))
                } else if let Obj::Null = env {
                    None
                } else {
                    panic!("{:?} not an environment object or null", env);
                }
            }),
            table: HashMap::new(),
        }
    }

    fn add_parent(&mut self, name: &str, entry: &Entry) -> bool {
        let mut curr_env = Some(self);
        let mut target_env = None;

        while let Some(env) = curr_env.take() {
            if let Some(_) = env.table.get_mut(name) {
                target_env = Some(env);
                break;
            }

            if let Some(ref mut parent) = env.parent {
                curr_env = Some(parent);
            } else {
                break;
            }
        }

        if let Some(env) = target_env {
            env.table.insert(name.to_owned(), entry.clone());
            true
        } else {
            false
        }
    }

    pub fn add(&mut self, name: &str, entry: Entry) {
        debug!("Adding name: {}, entry: {:?}", name, entry);
        if !self.add_parent(name, &entry) {
            self.table.insert(name.to_owned(), entry);
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
