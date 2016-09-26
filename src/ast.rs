use std::fs::File;
use std::io;
use std::io::prelude::Read;
use std::mem::transmute;

/// Reads a path to an AST file, parses the file contents into
/// the AST, and returns the root ScopeStmt
pub fn read_ast(path: String) -> io::Result<ScopeStmt> {
    let mut file = File::open(path)?;
    ScopeStmt::read(&mut file)
}

/// An expression
#[derive(Clone, Debug, PartialEq)]
pub enum Exp {
    Int(i32),
    Null,
    Printf(PrintfExp),
    Array(Box<ArrayExp>),
    Object(Box<ObjectExp>),
    Slot(Box<SlotExp>),
    SetSlot(Box<SetSlotExp>),
    CallSlot(Box<CallSlotExp>),
    Call(CallExp),
    Set(Box<SetExp>),
    If(Box<IfExp>),
    While(Box<WhileExp>),
    Ref(String),
}

impl Exp {
    /// Pretty prints an expression
    pub fn print(&self) {
        match *self {
            Exp::Int(i) => print!("{}", i),
            Exp::Null => print!("null"),
            Exp::Printf(ref printf) => {
                print!("printf({:?}", printf.format);
                for exp in &printf.exps {
                    print!(", ");
                    exp.print();
                }
                print!(")");
            }
            Exp::Array(ref array) => {
                print!("array(");
                array.length.print();
                print!(", ");
                array.init.print();
                print!(")");
            }
            Exp::Object(ref obj) => {
                print!("object : (");
                for slot in &obj.slots {
                    slot.print();
                }
                print!(")");
            }
            Exp::Slot(ref slot) => {
                slot.exp.print();
                print!(".{:?}", slot.name);
            }
            Exp::SetSlot(ref setslot) => {
                setslot.exp.print();
                print!(".{:?} = ", setslot.name);
                setslot.value.print();
            }
            Exp::CallSlot(ref callslot) => {
                callslot.exp.print();
                print!(".{:?}(", callslot.name);
                for (i, arg) in callslot.args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    arg.print();
                }
                print!(")");
            }
            Exp::Call(ref call) => {
                print!("{:?}(", call.name);
                for (i, arg) in call.args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    arg.print();
                }
                print!(")");
            }
            Exp::Set(ref set) => {
                print!("{:?} = ", set.name);
                set.exp.print();
            }
            Exp::If(ref iexp) => {
                print!("if ");
                iexp.pred.print();
                print!(" : (");
                iexp.conseq.print();
                print!(") else : (");
                iexp.alt.print();
                print!(")");
            }
            Exp::While(ref wexp) => {
                print!("while ");
                wexp.pred.print();
                print!(" : (");
                wexp.body.print();
                print!(")");
            }
            Exp::Ref(ref name) => print!("{:?}", name),
        }
    }

    /// Reads an expression from a file
    pub fn read(f: &mut File) -> io::Result<Exp> {
        let tag = read_int(f)?;
        match AstTag::from_i32(tag) {
            AstTag::IntExp => Ok(Exp::Int(read_int(f)?)),
            AstTag::NullExp => Ok(Exp::Null),
            AstTag::PrintfExp => {
                let format = read_string(f)?;
                let nexps = read_int(f)?;
                let exps = read_exps(f, nexps)?;

                Ok(Exp::Printf(PrintfExp {
                    format: format,
                    nexps: nexps,
                    exps: exps,
                }))
            }
            AstTag::ArrayExp => {
                let length = Exp::read(f)?;
                let init = Exp::read(f)?;

                Ok(Exp::Array(Box::new(ArrayExp {
                    length: length,
                    init: init,
                })))
            }
            AstTag::ObjectExp => {
                let parent = Exp::read(f)?;
                let nslots = read_int(f)?;
                let slots = read_slots(f, nslots)?;

                Ok(Exp::Object(Box::new(ObjectExp {
                    parent: parent,
                    nslots: nslots,
                    slots: slots,
                })))
            }
            AstTag::SlotExp => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;

                Ok(Exp::Slot(Box::new(SlotExp {
                    name: name,
                    exp: exp,
                })))
            }
            AstTag::SetSlotExp => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;
                let value = Exp::read(f)?;

                Ok(Exp::SetSlot(Box::new(SetSlotExp {
                    name: name,
                    exp: exp,
                    value: value,
                })))
            }
            AstTag::CallSlotExp => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;
                let nargs = read_int(f)?;
                let args = read_exps(f, nargs)?;

                Ok(Exp::CallSlot(Box::new(CallSlotExp {
                    name: name,
                    exp: exp,
                    nargs: nargs,
                    args: args,
                })))
            }
            AstTag::CallExp => {
                let name = read_string(f)?;
                let nargs = read_int(f)?;
                let args = read_exps(f, nargs)?;

                Ok(Exp::Call(CallExp {
                    name: name,
                    nargs: nargs,
                    args: args,
                }))
            }
            AstTag::SetExp => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;

                Ok(Exp::Set(Box::new(SetExp {
                    name: name,
                    exp: exp,
                })))
            }
            AstTag::IfExp => {
                let pred = Exp::read(f)?;
                let conseq = ScopeStmt::read(f)?;
                let alt = ScopeStmt::read(f)?;

                Ok(Exp::If(Box::new(IfExp {
                    pred: pred,
                    conseq: conseq,
                    alt: alt,
                })))
            }
            AstTag::WhileExp => {
                let pred = Exp::read(f)?;
                let body = ScopeStmt::read(f)?;

                Ok(Exp::While(Box::new(WhileExp {
                    pred: pred,
                    body: body,
                })))
            }
            AstTag::RefExp => Ok(Exp::Ref(read_string(f)?)),
            _ => Err(io::Error::new(io::ErrorKind::NotFound, "Invalid expression type")),
        }
    }
}

/// A slot statment (used inside an object)
#[derive(Clone, Debug, PartialEq)]
pub enum SlotStmt {
    Var(SlotVar),
    Method(SlotMethod),
}

impl SlotStmt {
    /// Pretty prints a slot statement
    pub fn print(&self) {
        match *self {
            SlotStmt::Var(ref vstmt) => {
                print!("var {:?} = ", vstmt.name);
                vstmt.exp.print();
            }
            SlotStmt::Method(ref method) => {
                print!("method {:?} (", method.name);
                for (i, arg) in method.args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{:?}", arg);
                }
                print!(") : (");
                method.body.print();
                print!(")");
            }
        }
    }

    /// Reads a slot statement from a file
    pub fn read(f: &mut File) -> io::Result<SlotStmt> {
        let tag = read_int(f)?;

        match AstTag::from_i32(tag) {
            AstTag::VarStmt => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;

                Ok(SlotStmt::Var(SlotVar {
                    name: name,
                    exp: exp,
                }))
            }
            AstTag::FnStmt => {
                let name = read_string(f)?;
                let nargs = read_int(f)?;
                let args = read_strings(f, nargs)?;
                let body = ScopeStmt::read(f)?;

                Ok(SlotStmt::Method(SlotMethod {
                    name: name,
                    nargs: nargs,
                    args: args,
                    body: body,
                }))
            }
            _ => Err(io::Error::new(io::ErrorKind::NotFound, "Invalid slot type")),
        }
    }
}

/// A scope statement (anything inside a block)
#[derive(Clone, Debug, PartialEq)]
pub enum ScopeStmt {
    Var(ScopeVar),
    Fn(Box<ScopeFn>),
    Seq(Box<ScopeSeq>),
    Exp(Exp),
}

impl ScopeStmt {
    /// Pretty prints a scope statement
    pub fn print(&self) {
        match *self {
            ScopeStmt::Var(ref varstmt) => {
                print!("var {:?} = ", varstmt.name);
                varstmt.exp.print();
            }
            ScopeStmt::Fn(ref fnstmt) => {
                print!("defn {:?} (", fnstmt.name);
                for (i, arg) in fnstmt.args.iter().enumerate() {
                    if i > 0 {
                        print!(", ");
                    }
                    print!("{:?}", arg);
                }
                print!(") : (");
                fnstmt.body.print();
                print!(")");
            }
            ScopeStmt::Seq(ref seq) => {
                seq.a.print();
                print!(" ");
                seq.b.print();
            }
            ScopeStmt::Exp(ref exp) => exp.print(),
        }
    }

    /// Reads a scope statement from a file
    pub fn read(f: &mut File) -> io::Result<ScopeStmt> {
        let tag = read_int(f)?;

        match AstTag::from_i32(tag) {
            AstTag::VarStmt => {
                let name = read_string(f)?;
                let exp = Exp::read(f)?;

                Ok(ScopeStmt::Var(ScopeVar {
                    name: name,
                    exp: exp,
                }))
            }
            AstTag::FnStmt => {
                let name = read_string(f)?;
                let nargs = read_int(f)?;
                let args = read_strings(f, nargs)?;
                let body = ScopeStmt::read(f)?;

                Ok(ScopeStmt::Fn(Box::new(ScopeFn {
                    name: name,
                    nargs: nargs,
                    args: args,
                    body: body,
                })))
            }
            AstTag::SeqStmt => {
                let a = ScopeStmt::read(f)?;
                let b = ScopeStmt::read(f)?;

                Ok(ScopeStmt::Seq(Box::new(ScopeSeq { a: a, b: b })))
            }
            AstTag::ExpStmt => Ok(ScopeStmt::Exp(Exp::read(f)?)),
            _ => Err(io::Error::new(io::ErrorKind::NotFound, "Invalid scope type")),
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct PrintfExp {
    pub format: String,
    pub nexps: i32,
    pub exps: Vec<Exp>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayExp {
    /// The length of the array as an integer expression
    pub length: Exp,
    /// The initial value to populate the array
    pub init: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectExp {
    pub parent: Exp,
    pub nslots: i32,
    pub slots: Vec<SlotStmt>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SlotExp {
    /// The name of the slot
    pub name: String,
    /// The expression that evaluates to the slot's object
    pub exp: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetSlotExp {
    /// The name of the slot
    pub name: String,
    /// The expression that evaluates to the slot's object
    pub exp: Exp,
    /// The value to set the slot
    pub value: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallSlotExp {
    pub name: String,
    pub exp: Exp,
    pub nargs: i32,
    pub args: Vec<Exp>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CallExp {
    pub name: String,
    pub nargs: i32,
    pub args: Vec<Exp>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SetExp {
    pub name: String,
    pub exp: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct IfExp {
    pub pred: Exp,
    pub conseq: ScopeStmt,
    pub alt: ScopeStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct WhileExp {
    pub pred: Exp,
    pub body: ScopeStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SlotVar {
    pub name: String,
    pub exp: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct SlotMethod {
    pub name: String,
    pub nargs: i32,
    pub args: Vec<String>,
    pub body: ScopeStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeVar {
    pub name: String,
    pub exp: Exp,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeFn {
    pub name: String,
    pub nargs: i32,
    pub args: Vec<String>,
    pub body: ScopeStmt,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ScopeSeq {
    pub a: ScopeStmt,
    pub b: ScopeStmt,
}

#[allow(dead_code)]
#[repr(i32)]
enum AstTag {
    IntExp = 0,
    NullExp,
    PrintfExp,
    ArrayExp,
    ObjectExp,
    SlotExp,
    SetSlotExp,
    CallSlotExp,
    CallExp,
    SetExp,
    IfExp,
    WhileExp,
    RefExp,
    VarStmt,
    FnStmt,
    SeqStmt,
    ExpStmt,
}

impl AstTag {
    fn from_i32(i: i32) -> AstTag {
        assert!(i >= AstTag::IntExp as i32 && i <= AstTag::ExpStmt as i32);
        unsafe { transmute(i) }
    }
}

fn read_int(f: &mut File) -> io::Result<i32> {
    let mut buf = [0; 4];
    f.read(&mut buf)?;
    let (b1, b2, b3, b4) = (buf[0] as i32, buf[1] as i32, buf[2] as i32, buf[3] as i32);
    Ok(b1 + (b2 << 8) + (b3 << 16) + (b4 << 24))
}

fn read_string(f: &mut File) -> io::Result<String> {
    let len = read_int(f)?;
    let mut buf = vec![0u8; len as usize];
    f.read(buf.as_mut_slice())?;
    String::from_utf8(buf).map_err(|_| io::Error::new(io::ErrorKind::InvalidData, "Invalid data"))
}

fn read_strings(f: &mut File, n: i32) -> io::Result<Vec<String>> {
    let mut strings = Vec::with_capacity(n as usize);
    for _ in 0..n {
        strings.push(read_string(f)?);
    }

    Ok(strings)
}

fn read_exps(f: &mut File, n: i32) -> io::Result<Vec<Exp>> {
    let mut exps = Vec::with_capacity(n as usize);
    for _ in 0..n {
        exps.push(Exp::read(f)?);
    }

    Ok(exps)
}

fn read_slots(f: &mut File, n: i32) -> io::Result<Vec<SlotStmt>> {
    let mut slots = Vec::with_capacity(n as usize);
    for _ in 0..n {
        slots.push(SlotStmt::read(f)?);
    }

    Ok(slots)
}
