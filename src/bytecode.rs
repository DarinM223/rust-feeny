use std::fs::File;
use std::io;
use std::io::prelude::Read;
use std::mem::transmute;

/// Reads a path to an bytecode file, parses the file contents into
/// the bytecode structure, and returns the program
pub fn load_bytecode(path: &str) -> io::Result<Program> {
    let mut file = try!(File::open(path));
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
    pub fn read(f: &mut File) -> io::Result<Value> {
        let tag = try!(read_byte(f));
        Ok(match ValTag::from_u8(tag) {
            ValTag::Int => Value::Int(try!(read_int(f))),
            ValTag::Null => Value::Null,
            ValTag::Str => Value::Str(try!(read_string(f))),
            ValTag::Method => {
                Value::Method(MethodValue {
                    name: try!(read_short(f)),
                    nargs: try!(read_byte(f)),
                    nlocals: try!(read_short(f)),
                    code: try!(read_code(f)),
                })
            }
            ValTag::Slot => Value::Slot(try!(read_short(f))),
            ValTag::Class => Value::Class(ClassValue { slots: try!(read_slots(f)) }),
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
    Label(i16),
    Lit(i16),
    Printf(i16, u8),
    Array,
    Object(i16),
    Slot(i16),
    SetSlot(i16),
    CallSlot(i16, u8),
    Call(i16, u8),
    SetLocal(i16),
    GetLocal(i16),
    SetGlobal(i16),
    GetGlobal(i16),
    Branch(i16),
    Goto(i16),
    Return,
    Drop,
}

impl Inst {
    /// Reads an instruction from a file
    pub fn read(f: &mut File) -> io::Result<Inst> {
        let op = try!(read_byte(f));
        Ok(match OpCode::from_u8(op) {
            OpCode::Label => Inst::Label(try!(read_short(f))),
            OpCode::Lit => Inst::Lit(try!(read_short(f))),
            OpCode::Printf => Inst::Printf(try!(read_short(f)), try!(read_byte(f))),
            OpCode::Array => Inst::Array,
            OpCode::Object => Inst::Object(try!(read_short(f))),
            OpCode::Slot => Inst::Slot(try!(read_short(f))),
            OpCode::SetSlot => Inst::SetSlot(try!(read_short(f))),
            OpCode::CallSlot => Inst::CallSlot(try!(read_short(f)), try!(read_byte(f))),
            OpCode::Call => Inst::CallSlot(try!(read_short(f)), try!(read_byte(f))),
            OpCode::SetLocal => Inst::SetLocal(try!(read_short(f))),
            OpCode::GetLocal => Inst::GetLocal(try!(read_short(f))),
            OpCode::SetGlobal => Inst::SetGlobal(try!(read_short(f))),
            OpCode::GetGlobal => Inst::GetGlobal(try!(read_short(f))),
            OpCode::Branch => Inst::Branch(try!(read_short(f))),
            OpCode::Goto => Inst::Goto(try!(read_short(f))),
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
    slots: Vec<i16>,
    entry: i16,
}

impl Program {
    /// Reads a program from a file
    pub fn read(f: &mut File) -> io::Result<Program> {
        Ok(Program {
            values: try!(read_values(f)),
            slots: try!(read_slots(f)),
            entry: try!(read_short(f)),
        })
    }

    /// Pretty prints a program
    pub fn print(&self) {
        println!("Constants: ");
        for (i, value) in self.values.iter().enumerate() {
            print!("#{}: ", i);
            value.print();
            print!("    ");
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
    name: i16,
    nargs: u8,
    nlocals: i16,
    code: Vec<Inst>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ClassValue {
    slots: Vec<i16>,
}

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

#[repr(u8)]
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

impl OpCode {
    fn from_u8(i: u8) -> OpCode {
        assert!(i >= OpCode::Label as u8 && i <= OpCode::Drop as u8);
        unsafe { transmute(i) }
    }
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

fn read_code(f: &mut File) -> io::Result<Vec<Inst>> {
    let n = try!(read_int(f));
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(try!(Inst::read(f)));
    }
    Ok(vec)
}

fn read_slots(f: &mut File) -> io::Result<Vec<i16>> {
    let n = try!(read_short(f));
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(try!(read_short(f)));
    }
    Ok(vec)
}

fn read_values(f: &mut File) -> io::Result<Vec<Value>> {
    let n = try!(read_short(f));
    let mut vec = Vec::with_capacity(n as usize);
    for _ in 0..n {
        vec.push(try!(Value::read(f)));
    }
    Ok(vec)
}
