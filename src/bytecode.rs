use std::fs::File;
use std::io;
use std::io::prelude::Read;
use std::mem::transmute;
use std::result;
use std::string::FromUtf8Error;

#[derive(Debug)]
pub enum BytecodeError {
    IoError(io::Error),
    Utf8Error(FromUtf8Error),
}

impl From<io::Error> for BytecodeError {
    fn from(err: io::Error) -> BytecodeError {
        BytecodeError::IoError(err)
    }
}

impl From<FromUtf8Error> for BytecodeError {
    fn from(err: FromUtf8Error) -> BytecodeError {
        BytecodeError::Utf8Error(err)
    }
}

pub type Result<T> = result::Result<T, BytecodeError>;

/// Reads a path to an bytecode file, parses the file contents into
/// the bytecode structure, and returns the program
pub fn load_bytecode(path: &str) -> Result<Program> {
    let mut file = File::open(path)?;
    Program::read(&mut file)
}

/// A bytecode value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Null,
    Str(String),
    Method(MethodValue),
    Slot(i16),
    Class(ClassValue),
}

impl Value {
    /// Reads a value from a file
    pub fn read(f: &mut File) -> Result<Value> {
        let tag = read_byte(f)?;
        Ok(match ValTag::from_u8(tag) {
            ValTag::Int => Value::Int(read_int(f)?),
            ValTag::Null => Value::Null,
            ValTag::Str => Value::Str(read_string(f)?),
            ValTag::Method => {
                Value::Method(MethodValue {
                    name: read_short(f)?,
                    nargs: read_byte(f)?,
                    nlocals: read_short(f)?,
                    code: read_code(f)?,
                })
            }
            ValTag::Slot => Value::Slot(read_short(f)?),
            ValTag::Class => Value::Class(ClassValue { slots: read_slots(f)? }),
        })
    }

    /// Pretty prints a value
    pub fn print(&self) {
        match *self {
            Value::Int(i) => print!("Int({})", i),
            Value::Null => print!("Null"),
            Value::Str(ref s) => print!("String({:?})", s),
            Value::Method(ref method) => {
                println!("Method(#{}, nargs:{}, nlocals:{})",
                         method.name,
                         method.nargs,
                         method.nlocals);

                for inst in &method.code {
                    print!("     ");
                    inst.print();
                    println!("");
                }
            }
            Value::Class(ref class) => {
                print!("Class(");
                for (i, slot) in class.slots.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }

                    print!("#{}", slot);
                }
                print!(")");
            }
            Value::Slot(i) => print!("Slot({})", i),
        }
    }
}

/// A bytecode instruction
#[derive(Clone, Debug, PartialEq)]
pub enum Inst {
    // Basic instructions
    Lit(i16),
    Array,
    Printf(i16, u8),
    SetLocal(i16),
    GetLocal(i16),
    SetGlobal(i16),
    GetGlobal(i16),
    Drop,

    // Object instructions
    Object(i16),
    GetSlot(i16),
    SetSlot(i16),
    CallSlot(i16, u8),

    // Control flow instructions
    Label(i16),
    Branch(i16),
    Goto(i16),
    Return,
    Call(i16, u8),
}

impl Inst {
    /// Reads an instruction from a file
    pub fn read(f: &mut File) -> Result<Inst> {
        let op = read_byte(f)?;
        Ok(match OpCode::from_u8(op) {
            OpCode::Label => Inst::Label(read_short(f)?),
            OpCode::Lit => Inst::Lit(read_short(f)?),
            OpCode::Printf => Inst::Printf(read_short(f)?, read_byte(f)?),
            OpCode::Array => Inst::Array,
            OpCode::Object => Inst::Object(read_short(f)?),
            OpCode::GetSlot => Inst::GetSlot(read_short(f)?),
            OpCode::SetSlot => Inst::SetSlot(read_short(f)?),
            OpCode::CallSlot => Inst::CallSlot(read_short(f)?, read_byte(f)?),
            OpCode::Call => Inst::Call(read_short(f)?, read_byte(f)?),
            OpCode::SetLocal => Inst::SetLocal(read_short(f)?),
            OpCode::GetLocal => Inst::GetLocal(read_short(f)?),
            OpCode::SetGlobal => Inst::SetGlobal(read_short(f)?),
            OpCode::GetGlobal => Inst::GetGlobal(read_short(f)?),
            OpCode::Branch => Inst::Branch(read_short(f)?),
            OpCode::Goto => Inst::Goto(read_short(f)?),
            OpCode::Return => Inst::Return,
            OpCode::Drop => Inst::Drop,
        })
    }

    /// Pretty prints an instruction
    pub fn print(&self) {
        match *self {
            Inst::Label(i) => print!("Label #{}", i),
            Inst::Lit(i) => print!("Lit #{}", i),
            Inst::Printf(format, arity) => print!("Printf #{} {}", format, arity),
            Inst::Array => print!("Array"),
            Inst::Object(i) => print!("Object #{}", i),
            Inst::GetSlot(i) => print!("Slot #{}", i),
            Inst::SetSlot(i) => print!("Set-slot #{}", i),
            Inst::CallSlot(name, arity) => print!("Call-slot #{} {}", name, arity),
            Inst::Call(name, arity) => print!("Call #{} {}", name, arity),
            Inst::SetLocal(i) => print!("Set-local {}", i),
            Inst::GetLocal(i) => print!("Get-local {}", i),
            Inst::SetGlobal(i) => print!("Set-global {}", i),
            Inst::GetGlobal(i) => print!("Get-global {}", i),
            Inst::Branch(i) => print!("Branch #{}", i),
            Inst::Goto(i) => print!("Goto #{}", i),
            Inst::Return => print!("Return"),
            Inst::Drop => print!("Drop"),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Program {
    pub values: Vec<Value>,
    pub slots: Vec<i16>,
    pub entry: i16,
    pub null_idx: i16,
    pub label_count: i32,
}

impl Program {
    /// Reads a program from a file
    pub fn read(f: &mut File) -> Result<Program> {
        Ok(Program {
            values: read_values(f)?,
            slots: read_slots(f)?,
            entry: read_short(f)?,
            null_idx: 0,
            label_count: 0,
        })
    }

    /// Pretty prints a program
    pub fn print(&self) {
        println!("Constants: ");
        for (i, value) in self.values.iter().enumerate() {
            print!("#{}: ", i);
            value.print();
            println!("");
        }
        println!("");
        println!("Globals: ");
        for slot in &self.slots {
            println!("#{}    ", slot);
        }
        println!("");
        println!("Entry: #{}", self.entry);
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct MethodValue {
    pub name: i16,
    pub nargs: u8,
    pub nlocals: i16,
    pub code: Vec<Inst>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassValue {
    pub slots: Vec<i16>,
}

#[allow(dead_code)]
#[repr(u8)]
enum ValTag {
    Int = 0,
    Null,
    Str,
    Method,
    Slot,
    Class,
}

impl ValTag {
    fn from_u8(i: u8) -> ValTag {
        assert!(i >= ValTag::Int as u8 && i <= ValTag::Class as u8);
        unsafe { transmute(i) }
    }
}

#[allow(dead_code)]
#[repr(u8)]
enum OpCode {
    Label = 0,
    Lit,
    Printf,
    Array,
    Object,
    GetSlot,
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

impl OpCode {
    fn from_u8(i: u8) -> OpCode {
        assert!(i >= OpCode::Label as u8 && i <= OpCode::Drop as u8);
        unsafe { transmute(i) }
    }
}

fn read_byte(f: &mut File) -> Result<u8> {
    Ok(f.bytes().next().unwrap()?)
}

fn read_short(f: &mut File) -> Result<i16> {
    let b1 = read_byte(f)? as i16;
    let b2 = read_byte(f)? as i16;

    Ok(b1 + (b2 << 8))
}

fn read_int(f: &mut File) -> Result<i32> {
    let b1 = read_byte(f)? as i32;
    let b2 = read_byte(f)? as i32;
    let b3 = read_byte(f)? as i32;
    let b4 = read_byte(f)? as i32;

    Ok(b1 + (b2 << 8) + (b3 << 16) + (b4 << 24))
}

fn read_string(f: &mut File) -> Result<String> {
    let len = read_int(f)?;
    let mut buf = Vec::with_capacity(len as usize);
    for _ in 0..len {
        buf.push(read_byte(f)?);
    }

    Ok(String::from_utf8(buf)?)
}

fn read_code(f: &mut File) -> Result<Vec<Inst>> {
    let n = read_int(f)?;
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(Inst::read(f)?);
    }
    Ok(vec)
}

fn read_slots(f: &mut File) -> Result<Vec<i16>> {
    let n = read_short(f)?;
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(read_short(f)?);
    }
    Ok(vec)
}

fn read_values(f: &mut File) -> Result<Vec<Value>> {
    let n = read_short(f)?;
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(Value::read(f)?);
    }
    Ok(vec)
}
