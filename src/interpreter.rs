use crate::ast::{Exp, ScopeStmt, SlotStmt};
use std::collections::HashMap;
use std::ops::{Add, Div, Index, Mul, Rem, Sub};
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
    stmt.eval(&mut Default::default(), &mut Obj::Null)?;
    Ok(())
}

/// A wrapper around an index into a Vec of environment
/// objects.
#[derive(Clone, Copy, Debug, PartialEq)]
pub struct EnvObjIdx(usize);

pub struct EnvironmentStore<T> {
    pub genv: EnvObj<T>,
    pub env_store: Vec<EnvObj<T>>,
}

impl<T> Default for EnvironmentStore<T> {
    fn default() -> Self {
        Self {
            genv: Default::default(),
            env_store: Default::default(),
        }
    }
}

impl<T> Index<EnvObjIdx> for EnvironmentStore<T> {
    type Output = EnvObj<T>;

    fn index(&self, index: EnvObjIdx) -> &Self::Output {
        &self.env_store[index.0]
    }
}

impl<T: Clone> EnvironmentStore<T> {
    pub fn from_table(globals: HashMap<String, T>) -> Self {
        Self {
            genv: EnvObj {
                parent: None,
                table: globals,
            },
            env_store: Default::default(),
        }
    }

    pub fn add_env(&mut self, env_obj: EnvObj<T>) -> EnvObjIdx {
        self.env_store.push(env_obj);
        EnvObjIdx(self.env_store.len() - 1)
    }

    /// Retrieves an entry from the environment.
    /// It first attempts to retrieve from the local environment and
    /// if that fails then it attempts to retrieve from the global environment
    pub fn get<'a>(&self, name: &str, env: &'a mut Obj) -> T {
        let mut ent = None;
        if let &mut Obj::Env(env_idx) = env {
            ent = self.env_store[env_idx.0].get(&self.env_store, name);
        }
        if ent.is_none() {
            ent = self.genv.get(&self.env_store, name);
        }

        ent.unwrap()
    }

    /// Sets an entry in the environment.
    /// If the local environment already contains the entry, it sets the local environment,
    /// otherwise it sets the global environment
    pub fn set(&mut self, name: &str, entry: &T, env: &mut Obj) -> Result<()> {
        let mut in_env = false;
        if let Obj::Env(env_idx) = *env {
            if self.env_store[env_idx.0].contains(&self.env_store, name) {
                self.add(env_idx, name, entry);
                in_env = true;
            }
        }

        if !in_env {
            // Only set if the object already contains the key
            let _ = self.genv.get_result(&self.env_store, name)?;
            self.genv.add(&mut self.env_store, name, entry.clone());
        }

        Ok(())
    }

    /// Adds key value pair to the environment at the given index.
    pub fn add(&mut self, env_idx: EnvObjIdx, name: &str, entry: &T) {
        let mut env_obj = std::mem::take(&mut self.env_store[env_idx.0]);
        env_obj.add(&mut self.env_store, name, entry.clone());
        self.env_store[env_idx.0] = env_obj;
    }

    /// Adds key value pair to global environment.
    pub fn add_global(&mut self, name: &str, entry: &T) {
        self.genv.add(&mut self.env_store, name, entry.clone());
    }
}

impl Exp {
    /// Evaluates an expression given a local and global environment
    pub fn eval(&self, store: &mut EnvironmentStore<Entry>, env: &mut Obj) -> Result<Obj> {
        match *self {
            Exp::Int(i) => Ok(Obj::Int(IntObj { value: i })),
            Exp::Null => Ok(Obj::Null),
            Exp::Printf(ref printf) => {
                let mut counter = 0;
                for ch in printf.format.chars() {
                    if ch == '~' {
                        let res = printf.exps[counter].eval(store, env)?;
                        print!("{}", res.int()?.value);
                        counter += 1;
                    } else {
                        print!("{}", ch);
                    }
                }
                Ok(Obj::Null)
            }
            Exp::Array(ref array) => {
                let length = array.length.eval(store, env)?;
                let init = array.init.eval(store, env)?;
                Ok(Obj::Array(ArrayObj::new(length, init)?))
            }
            Exp::Object(ref obj) => {
                let new_env = make_env_obj(Some(obj.parent.eval(store, env)?));
                let new_env_idx = store.add_env(new_env);
                let mut env_obj = Obj::Env(new_env_idx);
                for slot in &obj.slots {
                    slot.exec(store, env, &mut env_obj)?;
                }
                Ok(env_obj)
            }
            Exp::Slot(ref slot) => {
                debug!("Getting slot: {}", &slot.name[..]);
                let env_idx = slot.exp.eval(store, env)?.env()?;
                store[env_idx]
                    .get_result(&store.env_store, &slot.name[..])?
                    .var()
            }
            Exp::SetSlot(ref setslot) => {
                debug!("Setting slot: {}", &setslot.name[..]);
                let env_idx = setslot.exp.eval(store, env)?.env()?;
                let value = setslot.value.eval(store, env)?;
                store.add(env_idx, &setslot.name[..], &Entry::Var(value));

                Ok(Obj::Null)
            }
            Exp::CallSlot(ref cs) => {
                debug!("Calling slot: {}", &cs.name[..]);
                let mut obj = cs.exp.eval(store, env)?;
                match obj {
                    Obj::Int(iexp) => {
                        let other = cs.args[0].eval(store, env)?.int()?;
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
                        "length" => Ok(Obj::Int(arr.length())),
                        "set" => {
                            let name = cs.args[0].eval(store, env)?;
                            let param = cs.args[1].eval(store, env)?;
                            Ok(arr.set(name, param)?)
                        }
                        "get" => {
                            let name = cs.args[0].eval(store, env)?;
                            Ok(arr.get(name)?.unwrap_or(Obj::Null))
                        }
                        _ => Err(InterpretError::InvalidSlot),
                    },
                    Obj::Env(env_idx) => {
                        let (fun, args) = store[env_idx]
                            .get_result(&store.env_store, &cs.name[..])?
                            .clone()
                            .func()?;
                        if cs.nargs as usize != args.len() {
                            return Err(InterpretError::WrongArgsNum);
                        }

                        let mut new_env = EnvObj::new(None);
                        for (i, arg) in cs.args.iter().enumerate() {
                            let var_entry = Entry::Var(arg.eval(store, env)?);
                            new_env.add(&mut store.env_store, &args[i][..], var_entry);
                        }
                        new_env.add(&mut store.env_store, "this", Entry::Var(Obj::Env(env_idx)));

                        let mut env_entry = Obj::Env(store.add_env(new_env));
                        fun.eval(store, &mut env_entry)
                    }
                    _ => unreachable!(),
                }
            }
            Exp::Call(ref call) => {
                debug!("Calling function: {}", &call.name[..]);
                let (fun, args) = store
                    .genv
                    .get_result(&store.env_store, &call.name[..])?
                    .clone()
                    .func()?;
                if call.nargs as usize != args.len() {
                    return Err(InterpretError::WrongArgsNum);
                }

                let mut new_env = EnvObj::new(None);
                for (i, arg) in call.args.iter().enumerate() {
                    let var_entry = Entry::Var(arg.eval(store, env)?);
                    new_env.add(&mut store.env_store, &args[i][..], var_entry);
                }

                let new_env_idx = store.add_env(new_env);
                fun.eval(store, &mut Obj::Env(new_env_idx))
            }
            Exp::Set(ref set) => {
                let res = set.exp.eval(store, env)?;
                store.get(&set.name[..], env).var()?;
                store.set(&set.name, &Entry::Var(res), env)?;
                Ok(Obj::Null)
            }
            Exp::If(ref iexp) => {
                let pred = iexp.pred.eval(store, env)?;
                match pred {
                    Obj::Null => iexp.alt.eval(store, env),
                    _ => iexp.conseq.eval(store, env),
                }
            }
            Exp::While(ref wexp) => {
                while let Obj::Int(_) = wexp.pred.eval(store, env)? {
                    wexp.body.eval(store, env)?;
                }
                Ok(Obj::Null)
            }
            Exp::Ref(ref name) => Ok(store.get(name, env).var()?.clone()),
        }
    }
}

impl SlotStmt {
    /// Execute a slot statement given a local and global environment and
    /// the object that contains the slot
    pub fn exec(
        &self,
        store: &mut EnvironmentStore<Entry>,
        env: &mut Obj,
        obj: &mut Obj,
    ) -> Result<()> {
        let env_idx = obj.env()?;
        match *self {
            SlotStmt::Var(ref var) => {
                let var_exp = var.exp.eval(store, env)?;
                store.add(env_idx, &var.name[..], &Entry::Var(var_exp));
            }
            SlotStmt::Method(ref met) => {
                store.add(
                    env_idx,
                    &met.name[..],
                    &Entry::Func(met.body.clone(), met.args.clone()),
                );
            }
        }
        Ok(())
    }
}

impl ScopeStmt {
    /// Evaluates a scope statement given a local and global environment
    pub fn eval(&self, store: &mut EnvironmentStore<Entry>, env: &mut Obj) -> Result<Obj> {
        match *self {
            ScopeStmt::Var(ref var) => {
                debug!("Var: {}", &var.name[..]);
                let entry_obj = var.exp.eval(store, env)?;
                if let Obj::Env(env_idx) = *env {
                    store.add(env_idx, &var.name[..], &Entry::Var(entry_obj));
                } else {
                    store.add_global(&var.name[..], &Entry::Var(entry_obj));
                }
                Ok(Obj::Null)
            }
            ScopeStmt::Fn(ref fun) => {
                debug!("Function: {}", &fun.name[..]);
                store.add_global(
                    &fun.name[..],
                    &Entry::Func(fun.body.clone(), fun.args.clone()),
                );
                Ok(Obj::Null)
            }
            ScopeStmt::Seq(ref seq) => {
                seq.a.eval(store, env)?;
                Ok(seq.b.eval(store, env)?)
            }
            ScopeStmt::Exp(ref exp) => Ok(exp.eval(store, env)?),
        }
    }
}

/// An object used by the interpreter
#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Null,
    Int(IntObj),
    Array(ArrayObj),
    Env(EnvObjIdx),
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
    pub fn array(self) -> Result<ArrayObj> {
        match self {
            Obj::Array(arr) => Ok(arr),
            _ => Err(InterpretError::ObjShouldBeArray),
        }
    }

    /// Returns the unwrapped environment object as a Result<>
    #[inline]
    pub fn env(&self) -> Result<EnvObjIdx> {
        match self {
            Obj::Env(e) => Ok(*e),
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
    parent: Option<EnvObjIdx>,
    table: HashMap<String, T>,
}

impl<T> Default for EnvObj<T> {
    fn default() -> Self {
        Self {
            parent: Default::default(),
            table: Default::default(),
        }
    }
}

impl<T: Clone> EnvObj<T> {
    /// Creates a new environment object given a parent object
    pub fn new(parent: Option<EnvObjIdx>) -> EnvObj<T> {
        EnvObj {
            parent,
            table: HashMap::new(),
        }
    }

    /// Adds a new entry to the environment object
    pub fn add(&mut self, env_store: &mut Vec<EnvObj<T>>, name: &str, entry: T) {
        if !self.add_parent(env_store, name, &entry) {
            self.table.insert(name.to_owned(), entry);
        }
    }

    /// Retrieves an entry from the environment object
    pub fn get(&self, env_store: &Vec<EnvObj<T>>, name: &str) -> Option<T> {
        if let Some(data) = self.table.get(name) {
            return Some(data.clone());
        }

        let mut parent_env = self.parent;

        while let Some(env_idx) = parent_env {
            if let Some(data) = env_store[env_idx.0].get(env_store, name) {
                return Some(data.clone());
            }

            parent_env = env_store[env_idx.0].parent;
        }

        None
    }

    /// Same as get() but returns a Result<> instead of an Option<>
    #[inline]
    pub fn get_result(&self, env_store: &Vec<EnvObj<T>>, name: &str) -> Result<T> {
        match self.get(env_store, name) {
            Some(item) => Ok(item),
            None => Err(InterpretError::CannotFind(name.to_string())),
        }
    }

    /// Returns true if the object contains an entry with the given key,
    /// false otherwise
    pub fn contains(&self, env_store: &Vec<EnvObj<T>>, name: &str) -> bool {
        self.get(env_store, name).is_some()
    }

    /// Attempts to traverse up the parents looking for
    /// the first parent that contains the name
    /// and adds and sets the entry to that parent object.
    /// Returns whether the attempt was successful
    fn add_parent(&mut self, env_store: &mut Vec<EnvObj<T>>, name: &str, entry: &T) -> bool {
        if self.table.contains_key(name) {
            self.table.insert(name.to_owned(), entry.clone());
            return true;
        }

        let mut target_env = None;
        let mut parent_env = self.parent;

        while let Some(env_idx) = parent_env {
            let mut has_target_env = false;
            if env_store[env_idx.0].table.contains_key(name) {
                has_target_env = true;
            }
            if has_target_env {
                target_env = Some(env_idx);
                break;
            }

            parent_env = env_store[env_idx.0].parent;
        }

        if let Some(env_idx) = target_env {
            env_store[env_idx.0]
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
