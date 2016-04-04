use std::fs::File;
use std::io;
use std::io::prelude::Read;

/// A bytecode value
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Int(i32),
    Null,
    Str(String),
    Method(MethodValue),
    Slot(i32),
    Class(ClassValue),
}

impl Value {
    pub fn print(&self) {
        match *self {
            Value::Int(i) => print!("Int({})", i),
            Value::Null => print!("Null"),
            Value::Str(ref s) => print!("String({:?})", s),
            Value::Method(ref method) => {
                print!("Method(#{}, nargs:{}, nlocals:{})",
                       method.name,
                       method.nargs,
                       method.nlocals);

                for inst in &method.code {
                    println!("");
                    inst.print();
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
    Label(i32),
    Lit(i32),
    Printf(i32, i32),
    Array,
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
    Return,
    Drop,
}

impl Inst {
    pub fn print(&self) {
        match *self {
            Inst::Label(i) => print!("Label #{}", i),
            Inst::Lit(i) => print!("Lit #{}", i),
            Inst::Printf(format, arity) => print!("Printf #{} {}", format, arity),
            Inst::Array => print!("Array"),
            Inst::Object(i) => print!("Object #{}", i),
            Inst::Slot(i) => print!("Slot #{}", i),
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
        print!("Constants: ");
        for (i, value) in self.values.iter().enumerate() {
            println!("");
            print!("#{}: ", i);
            value.print();
        }
        println!("");
        println!("Globals: ");
        for slot in &self.slots {
            println!("#{}", slot);
        }
        println!("Entry: #{}", self.entry);
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

fn read_byte(f: &mut File) -> io::Result<u8> {
    f.bytes().next().unwrap()
}

fn read_short(f: &mut File) -> io::Result<i16> {
    let b1 = try!(read_byte(f)) as i16;
    let b2 = try!(read_byte(f)) as i16;

    Ok(b1 + (b2 << 8))
}

fn read_int(f: &mut File) -> io::Result<i32> {
    let b1 = try!(read_byte(f)) as i32;
    let b2 = try!(read_byte(f)) as i32;
    let b3 = try!(read_byte(f)) as i32;
    let b4 = try!(read_byte(f)) as i32;

    Ok(b1 + (b2 << 8) + (b3 << 16) + (b4 << 24))
}

fn read_string(f: &mut File) -> io::Result<String> {
    let len = try!(read_int(f));
    let mut buf = Vec::with_capacity(len as usize);
    for _ in 0..len {
        buf.push(try!(read_byte(f)));
    }

    String::from_utf8(buf).map_err(|_| {
        io::Error::new(io::ErrorKind::InvalidInput,
                       "Error converting utf8 to string")
    })
}
