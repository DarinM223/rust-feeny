use ast::{Exp, ScopeStmt, SlotStmt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::ops::{Add, Mul, Sub, Div, Rem};
use std::rc::Rc;

type Error = io::Error;

fn get_entry<'a>(name: &str, genv: &'a mut EnvObj, env: &'a mut Obj) -> Entry {
    let mut ent = None;
    if let &mut Obj::Env(ref mut env) = env {
        ent = env.borrow().get(name);
    }
    if ent == None {
        ent = genv.get(name);
    }

    ent.unwrap()
}

fn set_entry(name: &str, entry: &Entry, genv: &mut EnvObj, env: &mut Obj) -> io::Result<()> {
    use std::io::ErrorKind::*;
    let mut in_env = false;
    if let &mut Obj::Env(ref mut env) = env {
        debug!("Borrow 15!");
        if env.borrow().get(name) != None {
            debug!("Borrow 16!");
            env.borrow_mut().add(name, entry.clone());
            in_env = true;
        }
    }

    if !in_env {
        if let Some(_) = genv.get(name) {
            genv.add(name, entry.clone());
        } else {
            return Err(Error::new(InvalidData,
                                  "Both the current and global environments don't have the entry"));
        }
    }

    Ok(())
}

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
                Ok(Obj::Array(Rc::new(RefCell::new(ArrayObj::new(length, init)))))
            }
            Exp::Object(ref obj) => {
                debug!("Object!");
                let mut env_obj =
                    Obj::Env(Rc::new(RefCell::new(EnvObj::new(Some(try!(obj.parent
                                                                           .eval(genv, env)))))));
                for slot in &obj.slots {
                    slot.exec(genv, env, &mut env_obj);
                }
                Ok(env_obj)
            }
            Exp::Slot(ref slot) => {
                debug!("Getting slot!");
                let obj = try!(slot.exp.eval(genv, env));
                if let Obj::Env(env_obj) = obj {
                    debug!("Before borrow one!");
                    debug!("Environment object: {:?}", env_obj);
                    let entry = env_obj.borrow().get(&slot.name[..]);
                    if let Some(Entry::Var(obj)) = entry {
                        Ok(obj)
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
                if let Obj::Env(env_obj) = obj {
                    debug!("Before borrow two!");
                    debug!("Environment object: {:?}", env_obj);
                    env_obj.borrow_mut().add(&setslot.name[..], Entry::Var(value));
                    debug!("After!");
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

                        match &cs.name[..] {
                            "add" => Ok(Obj::Int(iexp + other)),
                            "sub" => Ok(Obj::Int(iexp - other)),
                            "mul" => Ok(Obj::Int(iexp * other)),
                            "div" => Ok(Obj::Int(iexp / other)),
                            "mod" => Ok(Obj::Int(iexp % other)),
                            "lt" => Ok(Obj::from_bool(iexp < other)),
                            "gt" => Ok(Obj::from_bool(iexp > other)),
                            "le" => Ok(Obj::from_bool(iexp <= other)),
                            "ge" => Ok(Obj::from_bool(iexp >= other)),
                            "eq" => Ok(Obj::from_bool(iexp == other)),
                            _ => Err(Error::new(InvalidInput, "Invalid slot")),
                        }
                    }
                    Obj::Array(ref mut arr) => {
                        match &cs.name[..] {
                            "length" => Ok(Obj::Int(arr.borrow().length())),
                            "set" => {
                                debug!("Borrow three!");
                                Ok(arr.borrow_mut().set(try!(cs.args[0].eval(genv, env)),
                                                        try!(cs.args[1].eval(genv, env))))
                            }
                            "get" => {
                                debug!("Borrow four!");
                                Ok(arr.borrow()
                                      .get(try!(cs.args[0].eval(genv, env)))
                                      .unwrap_or(Obj::Null))
                            }
                            _ => Err(Error::new(InvalidInput, "Invalid slot")),
                        }
                    }
                    Obj::Env(ref mut ent) => {
                        let ent_clone = ent.clone();
                        debug!("Entry: {:?}", ent);
                        debug!("Borrow five!");
                        if let Some(Entry::Func(ref fun, ref args)) = ent.borrow()
                                                                         .get(&cs.name[..]) {
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

                            fun.eval(genv, &mut Obj::Env(Rc::new(RefCell::new(new_env))))
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
                    Some(Entry::Func(ref fun, ref args)) => (fun.clone(), args.clone()),
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

                fun.eval(genv, &mut Obj::Env(Rc::new(RefCell::new(new_env))))
            }
            Exp::Set(ref set) => {
                debug!("Setting!");
                let res = try!(set.exp.eval(genv, env));
                let ent = get_entry(&set.name[..], genv, env);

                match ent {
                    Entry::Var(_) => set_entry(&set.name, &Entry::Var(res), genv, env),
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

                match ent {
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
                    debug!("Borrow 6!");
                    let var_exp = try!(var.exp.eval(genv, env));
                    env_obj.borrow_mut().add(&var.name[..], Entry::Var(var_exp));
                }
                SlotStmt::Method(ref met) => {
                    debug!("Borrow 7!");
                    env_obj.borrow_mut().add(&met.name[..],
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
                    debug!("Borrow 8!");
                    env_obj.borrow_mut().add(&var.name[..], Entry::Var(entry_obj));
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
#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Null,
    Int(IntObj),
    Array(Rc<RefCell<ArrayObj>>),
    Env(Rc<RefCell<EnvObj>>),
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

    pub fn from_bool(b: bool) -> Obj {
        match b {
            true => Obj::Int(IntObj { value: 1 }),
            false => Obj::Null,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, PartialOrd, Ord)]
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

#[derive(Clone, Debug, PartialEq)]
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

    pub fn get(&self, i: Obj) -> Option<Obj> {
        let index = match i {
            Obj::Int(i) => i.value,
            _ => unreachable!(),
        };
        self.arr.get(index as usize).map(|o| o.clone())
    }
}

/// Environment entries
#[derive(Clone, Debug, PartialEq)]
pub enum Entry {
    Var(Obj),
    Func(ScopeStmt, Vec<String>),
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnvObj {
    parent: Option<Box<Rc<RefCell<EnvObj>>>>,
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
        if self.table.get_mut(name) != None {
            self.table.insert(name.to_owned(), entry.clone());
            return true;
        }

        let mut target_env = None;
        let mut parent_env = self.parent.clone();

        while let Some(env) = parent_env.take() {
            let mut has_target_env = false;
            {
                debug!("Borrow 9!");
                if env.borrow().table.get(name) != None {
                    has_target_env = true;
                }
            }
            if has_target_env {
                target_env = Some(env);
                break;
            }

            debug!("Borrow 10!");
            if let Some(ref mut parent) = env.borrow_mut().parent {
                parent_env = Some(parent.clone());
            } else {
                break;
            }
        }

        if let Some(ref mut env) = target_env {
            debug!("Borrow 11!");
            env.borrow_mut().table.insert(name.to_owned(), entry.clone());
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

    pub fn get(&self, name: &str) -> Option<Entry> {
        if let Some(data) = self.table.get(name) {
            return Some(data.clone());
        }

        let mut parent_env = self.parent.clone();

        while let Some(env) = parent_env.take() {
            debug!("Borrow 12!");
            if let Some(data) = env.borrow().get(name) {
                return Some(data.clone());
            }

            debug!("Borrow 13!");
            if let Some(ref parent) = env.borrow().parent {
                parent_env = Some(parent.clone());
            } else {
                break;
            }
        }

        None
    }
}

#[cfg(test)]
mod test_int_obj {
    use super::*;

    #[test]
    fn test_boolean_ops() {
        assert!(IntObj { value: 2 } > IntObj { value: 1 });
        assert!(IntObj { value: 1 } < IntObj { value: 2 });
        assert!(IntObj { value: -1 } < IntObj { value: 0 });
        assert!(IntObj { value: 1 } == IntObj { value: 1 });
    }

    #[test]
    fn test_number_ops() {
        assert_eq!(IntObj { value: 2 } + IntObj { value: 3 },
                   IntObj { value: 5 });
        assert_eq!(IntObj { value: 2 } - IntObj { value: 3 },
                   IntObj { value: -1 });
        assert_eq!(IntObj { value: 2 } * IntObj { value: 3 },
                   IntObj { value: 6 });
    }
}

#[cfg(test)]
mod test_array_obj {
    use super::*;

    #[test]
    fn test_init() {
        let length = Obj::Int(IntObj { value: 3 });
        let init = Obj::Int(IntObj { value: 69 });
        let init_clone = init.clone();
        assert_eq!(ArrayObj::new(length, init),
                   ArrayObj {
                       length: IntObj { value: 3 },
                       arr: vec![init_clone; 3],
                   });
    }

    #[test]
    #[should_panic]
    fn test_init_wrong_param() {
        ArrayObj::new(Obj::Null, Obj::Int(IntObj { value: 69 }));
    }
}
