use ast::{Exp, ScopeStmt, SlotStmt};
use std::cell::RefCell;
use std::collections::HashMap;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::rc::Rc;
use std::result;

#[derive(Debug, PartialEq)]
pub enum InterpretError {
    InvalidSlot,
    WrongArgsNum,
    ObjShouldBeNull,
    ObjShouldBeInt,
    ObjShouldBeArray,
    ObjShouldBeObject,
    EntryShouldBeVar,
    EntryShouldBeMethod,
    CannotFind(String),
}

pub type Result<T> = result::Result<T, InterpretError>;

/// Interprets an AST of a program
pub fn interpret(stmt: ScopeStmt) -> Result<()> {
    stmt.eval(&mut EnvObj::new(None), &mut Obj::Null)?;
    Ok(())
}

impl Exp {
    /// Evaluates an expression given a local and global environment
    pub fn eval(&self, genv: &mut EnvObj<Entry>, env: &mut Obj) -> Result<Obj> {
        match *self {
            Exp::Int(i) => Ok(Obj::Int(IntObj { value: i })),
            Exp::Null => Ok(Obj::Null),
            Exp::Printf(ref printf) => {
                let mut counter = 0;
                for ch in printf.format.chars() {
                    if ch == '~' {
                        let res = printf.exps[counter].eval(genv, env)?;
                        print!("{}", res.int()?.value);
                        counter += 1;
                    } else {
                        print!("{}", ch);
                    }
                }
                Ok(Obj::Null)
            }
            Exp::Array(ref array) => {
                let length = array.length.eval(genv, env)?;
                let init = array.init.eval(genv, env)?;
                Ok(Obj::Array(Rc::new(RefCell::new(ArrayObj::new(
                    length, init,
                )?))))
            }
            Exp::Object(ref obj) => {
                let mut env_obj = Obj::Env(Rc::new(RefCell::new(make_env_obj(Some(
                    obj.parent.eval(genv, env)?,
                )))));
                for slot in &obj.slots {
                    slot.exec(genv, env, &mut env_obj)?;
                }
                Ok(env_obj)
            }
            Exp::Slot(ref slot) => {
                debug!("Getting slot: {}", &slot.name[..]);
                let env_obj = slot.exp.eval(genv, env)?.env()?;
                let var_slot = env_obj.borrow().get_result(&slot.name[..])?.var();
                var_slot
            }
            Exp::SetSlot(ref setslot) => {
                debug!("Setting slot: {}", &setslot.name[..]);
                let env_obj = setslot.exp.eval(genv, env)?.env()?;
                let value = setslot.value.eval(genv, env)?;
                env_obj
                    .borrow_mut()
                    .add(&setslot.name[..], Entry::Var(value));

                Ok(Obj::Null)
            }
            Exp::CallSlot(ref cs) => {
                debug!("Calling slot: {}", &cs.name[..]);
                let mut obj = cs.exp.eval(genv, env)?;
                match obj {
                    Obj::Int(iexp) => {
                        let other = cs.args[0].eval(genv, env)?.int()?;
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
                            _ => Err(InterpretError::InvalidSlot),
                        }
                    }
                    Obj::Array(ref mut arr) => match &cs.name[..] {
                        "length" => Ok(Obj::Int(arr.borrow().length())),
                        "set" => {
                            let name = cs.args[0].eval(genv, env)?;
                            let param = cs.args[1].eval(genv, env)?;
                            Ok(arr.borrow_mut().set(name, param)?)
                        }
                        "get" => {
                            let name = cs.args[0].eval(genv, env)?;
                            Ok(arr.borrow().get(name)?.unwrap_or(Obj::Null))
                        }
                        _ => Err(InterpretError::InvalidSlot),
                    },
                    Obj::Env(ref mut ent) => {
                        let (fun, args) = ent.borrow().get_result(&cs.name[..])?.clone().func()?;
                        if cs.nargs as usize != args.len() {
                            return Err(InterpretError::WrongArgsNum);
                        }

                        let mut new_env = EnvObj::new(None);
                        for (i, arg) in cs.args.iter().enumerate() {
                            new_env.add(&args[i][..], Entry::Var(arg.eval(genv, env)?));
                        }
                        new_env.add("this", Entry::Var(Obj::Env(ent.clone())));

                        fun.eval(genv, &mut Obj::Env(Rc::new(RefCell::new(new_env))))
                    }
                    _ => unreachable!(),
                }
            }
            Exp::Call(ref call) => {
                debug!("Calling function: {}", &call.name[..]);
                let (fun, args) = genv.get_result(&call.name[..])?.clone().func()?;
                if call.nargs as usize != args.len() {
                    return Err(InterpretError::WrongArgsNum);
                }

                let mut new_env = EnvObj::new(None);
                for (i, arg) in call.args.iter().enumerate() {
                    new_env.add(&args[i][..], Entry::Var(arg.eval(genv, env)?));
                }

                fun.eval(genv, &mut Obj::Env(Rc::new(RefCell::new(new_env))))
            }
            Exp::Set(ref set) => {
                let res = set.exp.eval(genv, env)?;
                get_entry(&set.name[..], genv, env).var()?;
                set_entry(&set.name, &Entry::Var(res), genv, env)?;
                Ok(Obj::Null)
            }
            Exp::If(ref iexp) => {
                let pred = iexp.pred.eval(genv, env)?;
                match pred {
                    Obj::Null => iexp.alt.eval(genv, env),
                    _ => iexp.conseq.eval(genv, env),
                }
            }
            Exp::While(ref wexp) => {
                while let Obj::Int(_) = wexp.pred.eval(genv, env)? {
                    wexp.body.eval(genv, env)?;
                }
                Ok(Obj::Null)
            }
            Exp::Ref(ref name) => Ok(get_entry(name, genv, env).var()?.clone()),
        }
    }
}

impl SlotStmt {
    /// Execute a slot statement given a local and global environment and
    /// the object that contains the slot
    pub fn exec(&self, genv: &mut EnvObj<Entry>, env: &mut Obj, obj: &mut Obj) -> Result<()> {
        let env_obj = obj.env_mut()?;
        match *self {
            SlotStmt::Var(ref var) => {
                let var_exp = var.exp.eval(genv, env)?;
                env_obj.borrow_mut().add(&var.name[..], Entry::Var(var_exp));
            }
            SlotStmt::Method(ref met) => {
                env_obj.borrow_mut().add(
                    &met.name[..],
                    Entry::Func(met.body.clone(), met.args.clone()),
                );
            }
        }
        Ok(())
    }
}

impl ScopeStmt {
    /// Evaluates a scope statement given a local and global environment
    pub fn eval(&self, genv: &mut EnvObj<Entry>, env: &mut Obj) -> Result<Obj> {
        match *self {
            ScopeStmt::Var(ref var) => {
                debug!("Var: {}", &var.name[..]);
                let entry_obj = var.exp.eval(genv, env)?;
                if let Obj::Env(ref mut env_obj) = *env {
                    env_obj
                        .borrow_mut()
                        .add(&var.name[..], Entry::Var(entry_obj));
                } else {
                    genv.add(&var.name[..], Entry::Var(entry_obj));
                }
                Ok(Obj::Null)
            }
            ScopeStmt::Fn(ref fun) => {
                debug!("Function: {}", &fun.name[..]);
                genv.add(
                    &fun.name[..],
                    Entry::Func(fun.body.clone(), fun.args.clone()),
                );
                Ok(Obj::Null)
            }
            ScopeStmt::Seq(ref seq) => {
                seq.a.eval(genv, env)?;
                Ok(seq.b.eval(genv, env)?)
            }
            ScopeStmt::Exp(ref exp) => Ok(exp.eval(genv, env)?),
        }
    }
}

pub type EnvObjRef<T> = Rc<RefCell<EnvObj<T>>>;

/// An object used by the interpreter
#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Null,
    Int(IntObj),
    Array(Rc<RefCell<ArrayObj>>),
    Env(EnvObjRef<Entry>),
}

impl Obj {
    /// Returns the type of the object as an integer
    pub fn obj_type(&self) -> i32 {
        match *self {
            Obj::Null => 0,
            Obj::Int(_) => 1,
            Obj::Array(_) => 2,
            Obj::Env(_) => 3,
        }
    }

    /// Creates an Integer object if true, otherwise creates a Null object
    pub fn from_bool(b: bool) -> Obj {
        match b {
            true => Obj::Int(IntObj { value: 0 }),
            false => Obj::Null,
        }
    }

    /// Returns a Result<> containing nothing if object is null, or an error
    /// if the object is not a null object
    #[inline]
    pub fn null(self) -> Result<()> {
        match self {
            Obj::Null => Ok(()),
            _ => Err(InterpretError::ObjShouldBeNull),
        }
    }

    /// Returns the unwrapped integer object as a Result<>
    #[inline]
    pub fn int(self) -> Result<IntObj> {
        match self {
            Obj::Int(obj) => Ok(obj),
            _ => Err(InterpretError::ObjShouldBeInt),
        }
    }

    /// Returns the unwrapped array object as a Result<>
    #[inline]
    pub fn array(self) -> Result<Rc<RefCell<ArrayObj>>> {
        match self {
            Obj::Array(arr) => Ok(arr),
            _ => Err(InterpretError::ObjShouldBeArray),
        }
    }

    /// Returns the unwrapped environment object as a Result<>
    #[inline]
    pub fn env(self) -> Result<EnvObjRef<Entry>> {
        match self {
            Obj::Env(e) => Ok(e),
            _ => Err(InterpretError::ObjShouldBeObject),
        }
    }

    /// Returns a mutable reference to the unwrapped environment object as a Result<>
    #[inline]
    pub fn env_mut<'a>(&'a mut self) -> Result<&'a mut EnvObjRef<Entry>> {
        match *self {
            Obj::Env(ref mut e) => Ok(e),
            _ => Err(InterpretError::ObjShouldBeObject),
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
        IntObj {
            value: self.value + other.value,
        }
    }
}

impl Sub<IntObj> for IntObj {
    type Output = IntObj;

    fn sub(self, other: IntObj) -> IntObj {
        IntObj {
            value: self.value - other.value,
        }
    }
}

impl Mul<IntObj> for IntObj {
    type Output = IntObj;

    fn mul(self, other: IntObj) -> IntObj {
        IntObj {
            value: self.value * other.value,
        }
    }
}

impl Div<IntObj> for IntObj {
    type Output = IntObj;

    fn div(self, other: IntObj) -> IntObj {
        IntObj {
            value: self.value / other.value,
        }
    }
}

impl Rem<IntObj> for IntObj {
    type Output = IntObj;

    fn rem(self, other: IntObj) -> IntObj {
        IntObj {
            value: self.value % other.value,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayObj {
    length: IntObj,
    arr: Vec<Obj>,
}

impl ArrayObj {
    /// Creates a new array given the length and an initial value to
    /// populate the array
    pub fn new(length: Obj, init: Obj) -> Result<ArrayObj> {
        let len = length.int()?.value;

        Ok(ArrayObj {
            length: IntObj { value: len },
            arr: vec![init; len as usize],
        })
    }

    /// Returns the length of the array
    pub fn length(&self) -> IntObj {
        self.length.clone()
    }

    /// Sets a value in the array
    pub fn set(&mut self, index: Obj, val: Obj) -> Result<Obj> {
        if let Some(item) = self.arr.get_mut(index.int()?.value as usize) {
            *item = val;
        }
        Ok(Obj::Null)
    }

    /// Retrieves a value from the array
    pub fn get(&self, index: Obj) -> Result<Option<Obj>> {
        Ok(self.arr.get(index.int()?.value as usize).cloned())
    }
}

/// Environment entries
#[derive(Clone, Debug, PartialEq)]
pub enum Entry {
    /// A variable entry
    Var(Obj),
    /// A method entry
    Func(ScopeStmt, Vec<String>),
}

impl Entry {
    /// Returns the unwrapped variable entry as a Result<>
    #[inline]
    pub fn var(self) -> Result<Obj> {
        match self {
            Entry::Var(obj) => Ok(obj),
            _ => Err(InterpretError::EntryShouldBeVar),
        }
    }

    /// Returns the unwrapped method entry as a Result<>
    #[inline]
    pub fn func(self) -> Result<(ScopeStmt, Vec<String>)> {
        match self {
            Entry::Func(stmt, strs) => Ok((stmt, strs)),
            _ => Err(InterpretError::EntryShouldBeMethod),
        }
    }
}

/// A generic environment object implementation that allows
/// for prototypical inheritance (like Javascript)
#[derive(Clone, Debug, PartialEq)]
pub struct EnvObj<T> {
    parent: Option<Box<EnvObjRef<T>>>,
    table: HashMap<String, T>,
}

impl<T: Clone + PartialEq> EnvObj<T> {
    /// Creates a new environment object given a parent object
    pub fn new(parent: Option<EnvObjRef<T>>) -> EnvObj<T> {
        EnvObj {
            parent: parent.map(|parent| Box::new(parent)),
            table: HashMap::new(),
        }
    }

    /// Adds a new entry to the environment object
    pub fn add(&mut self, name: &str, entry: T) {
        if !self.add_parent(name, &entry) {
            self.table.insert(name.to_owned(), entry);
        }
    }

    /// Retrieves an entry from the environment object
    pub fn get(&self, name: &str) -> Option<T> {
        if let Some(data) = self.table.get(name) {
            return Some(data.clone());
        }

        let mut parent_env = self.parent.clone();

        while let Some(env) = parent_env.take() {
            if let Some(data) = env.borrow().get(name) {
                return Some(data.clone());
            }

            if let Some(ref parent) = env.borrow().parent {
                parent_env = Some(parent.clone());
            } else {
                break;
            }
        }

        None
    }

    /// Same as get() but returns a Result<> instead of an Option<>
    #[inline]
    pub fn get_result(&self, name: &str) -> Result<T> {
        match self.get(name) {
            Some(item) => Ok(item),
            None => Err(InterpretError::CannotFind(name.to_string())),
        }
    }

    /// Returns true if the object contains an entry with the given key,
    /// false otherwise
    pub fn contains(&self, name: &str) -> bool {
        match self.get(name) {
            Some(_) => true,
            _ => false,
        }
    }

    /// Attempts to traverse up the parents looking for
    /// the first parent that contains the name
    /// and adds and sets the entry to that parent object.
    /// Returns whether the attempt was successful
    fn add_parent(&mut self, name: &str, entry: &T) -> bool {
        if self.table.get_mut(name) != None {
            self.table.insert(name.to_owned(), entry.clone());
            return true;
        }

        let mut target_env = None;
        let mut parent_env = self.parent.clone();

        while let Some(env) = parent_env.take() {
            let mut has_target_env = false;
            {
                if env.borrow().table.get(name) != None {
                    has_target_env = true;
                }
            }
            if has_target_env {
                target_env = Some(env);
                break;
            }

            if let Some(ref mut parent) = env.borrow_mut().parent {
                parent_env = Some(parent.clone());
            } else {
                break;
            }
        }

        if let Some(ref mut env) = target_env {
            env.borrow_mut()
                .table
                .insert(name.to_owned(), entry.clone());
            true
        } else {
            false
        }
    }
}

/// Create a new environment object from a parent interpreter object
fn make_env_obj(parent: Option<Obj>) -> EnvObj<Entry> {
    match parent {
        Some(Obj::Env(env)) => EnvObj::new(Some(env)),
        Some(Obj::Null) => EnvObj::new(None),
        _ => panic!("{:?} not an environment object or null", parent),
    }
}

/// Retrieves an entry from the environment.
/// It first attempts to retrieve from the local environment and
/// if that fails then it attempts to retrieve from the global environment
fn get_entry<'a>(name: &str, genv: &'a mut EnvObj<Entry>, env: &'a mut Obj) -> Entry {
    let mut ent = None;
    if let &mut Obj::Env(ref mut env) = env {
        ent = env.borrow().get(name);
    }
    if ent == None {
        ent = genv.get(name);
    }

    ent.unwrap()
}

/// Sets an entry in the environment.
/// If the local environment already contains the entry, it sets the local environment,
/// otherwise it sets the global environment
fn set_entry(name: &str, entry: &Entry, genv: &mut EnvObj<Entry>, env: &mut Obj) -> Result<()> {
    let mut in_env = false;
    if let &mut Obj::Env(ref mut env) = env {
        if env.borrow().get(name) != None {
            env.borrow_mut().add(name, entry.clone());
            in_env = true;
        }
    }

    if !in_env {
        // Only set if the object already contains the key
        let _ = genv.get_result(name)?;
        genv.add(name, entry.clone());
    }

    Ok(())
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
        assert_eq!(
            IntObj { value: 2 } + IntObj { value: 3 },
            IntObj { value: 5 }
        );
        assert_eq!(
            IntObj { value: 2 } - IntObj { value: 3 },
            IntObj { value: -1 }
        );
        assert_eq!(
            IntObj { value: 2 } * IntObj { value: 3 },
            IntObj { value: 6 }
        );
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
        assert_eq!(
            ArrayObj::new(length, init),
            Ok(ArrayObj {
                length: IntObj { value: 3 },
                arr: vec![init_clone; 3],
            })
        );
    }

    #[test]
    fn test_init_wrong_param() {
        assert!(ArrayObj::new(Obj::Null, Obj::Int(IntObj { value: 69 })).is_err());
    }
}
