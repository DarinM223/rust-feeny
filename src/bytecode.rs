/// A bytecode value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Str(String),
    Method(MethodValue),
    Slot(i32),
    Class(ClassValue),
}

/// A bytecode instruction
#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    Label(i32),
    Lit(i32),
    Printf(i32, i32),
    Object(i32),
    Slot(i32),
    SetSlot(i32),
    CallSlot(i32, i32),
    Call(i32, i32),
    SetLocal(i32),
    GetLocal(i32),
    SetGlobal(i32),
    GetGlobal(i32),
    Branch(i32),
    Goto(i32),
}

impl Inst {
    pub fn print(&self) {
        // TODO(DarinM223): implement this
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    values: Vec<Value>,
    slots: Vec<i32>,
    entry: i32,
}

impl Program {
    pub fn load_bytecode(path: &str) -> Program {
        // TODO(DarinM223): implement this
        Program {
            values: Vec::new(),
            slots: Vec::new(),
            entry: -1,
        }
    }

    pub fn print(&self) {
        // TODO(DarinM223): implement this
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodValue {
    name: i32,
    nargs: i32,
    nlocals: i32,
    code: Vec<Inst>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassValue {
    slots: Vec<i32>,
}

#[repr(i32)]
enum ValTag {
    Int = 0,
    Null,
    Str,
    Method,
    Slot,
    Class,
}

#[repr(i32)]
enum OpCode {
    Label = 0,
    Lit,
    Printf,
    Array,
    Object,
    Slot,
    SetSlot,
    CallSlot,
    Call,
    SetLocal,
    GetLocal,
    SetGlobal,
    GetGlobal,
    Branch,
    Goto,
    Return,
    Drop,
}
