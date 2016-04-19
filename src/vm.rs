use bytecode::{Inst, MethodValue, Program, Value};
use std::collections::HashMap;
use std::io;
use std::mem;

/// Interprets the bytecode structure
pub fn interpret_bc(program: Program) -> io::Result<()> {
    let mut vm = try!(VM::new(program));

    // TODO(DarinM223): implement this
    while vm.pc < vm.code.len() as i32 {
        let inst = vm.code.get(vm.pc as usize).unwrap();
        match *inst {
            Inst::Lit(idx) => {}
            Inst::Array => {}
            Inst::Printf(format, arity) => {}
            Inst::Object(class) => {}
            Inst::Slot(name) => {}
            Inst::SetSlot(name) => {}
            Inst::CallSlot(name, arity) => {}
            Inst::SetLocal(idx) => {}
            Inst::GetLocal(idx) => {}
            Inst::SetGlobal(name) => {}
            Inst::GetGlobal(name) => {}
            Inst::Drop => {}
            Inst::Label(name) => {}
            Inst::Branch(name) => {}
            Inst::Goto(name) => {}
            Inst::Call(name, arity) => {}
            Inst::Return => {}
        }

        vm.pc += 1;
    }

    Ok(())
}

#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Int(i64),
    Null,
    Array(ArrayObj),
    Object(EnvObj),
    Method(MethodValue),
}

impl Obj {
    pub fn obj_type(&self) -> i32 {
        match *self {
            Obj::Int(_) => 0,
            Obj::Null => 1,
            Obj::Array(_) => 2,
            Obj::Object(_) => 3,
            Obj::Method(_) => 4,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct ArrayObj {
    pub length: i64,
    pub slots: Vec<Obj>,
}

impl ArrayObj {
    pub fn new(length: Obj, init: Obj) -> ArrayObj {
        let length = match length {
            Obj::Int(len) => len,
            _ => unreachable!(),
        };

        ArrayObj {
            length: length,
            slots: vec![init; length as usize],
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct EnvObj {
    pub parent: Option<Box<Obj>>,
    pub nslots: i32,
    pub table: HashMap<String, Obj>,
}

impl EnvObj {
    pub fn new(nslots: i32, parent: Option<Box<Obj>>) -> EnvObj {
        EnvObj {
            parent: parent,
            nslots: nslots,
            table: HashMap::with_capacity(nslots as usize),
        }
    }
}

#[derive(Clone, Debug)]
struct LabelAddr {
    code: Option<Vec<Inst>>,
    pc: i32,
}

#[derive(Clone, Debug)]
pub struct Frame {
    slots: Vec<Obj>,
    ret_addr: LabelAddr,
    parent: Option<Box<Frame>>,
}

impl Frame {
    pub fn new(code: Option<Vec<Inst>>,
               pc: i32,
               num_slots: i32,
               parent: Option<Box<Frame>>)
               -> Frame {
        let slots = Vec::with_capacity(num_slots as usize);
        let ret_addr = LabelAddr {
            code: code,
            pc: pc,
        };

        Frame {
            slots: slots,
            ret_addr: ret_addr,
            parent: parent,
        }
    }
}

pub const VM_CAPACITY: usize = 13;

pub struct VM {
    global_vars: HashMap<String, Obj>,
    labels: HashMap<String, LabelAddr>,
    code: Vec<Inst>,
    pc: i32,
    operand: Vec<Obj>,
    local_frame: Frame,
}

impl VM {
    pub fn new(mut p: Program) -> io::Result<VM> {
        use std::io::ErrorKind::*;

        let mut global_vars = HashMap::with_capacity(VM_CAPACITY);
        let mut labels = HashMap::with_capacity(VM_CAPACITY);
        let local_frame;
        let code;

        if let Some(&mut Value::Method(ref mut entry_func)) = p.values.get_mut(p.entry as usize) {
            local_frame = Frame::new(None,
                                     0,
                                     (entry_func.nargs as i32) + (entry_func.nlocals as i32),
                                     None);
            code = mem::replace(&mut entry_func.code, vec![]);
        } else {
            return Err(io::Error::new(InvalidInput, "Entry function needs to be a method value!"));
        }

        // Initialize global variables
        for idx in p.slots {
            let value = p.values.get(idx as usize).unwrap();
            match *value {
                Value::Method(ref m) => {
                    let name = match p.values[m.name as usize] {
                        Value::Str(ref s) => s.clone(),
                        _ => return Err(io::Error::new(InvalidData, "Invalid object type")),
                    };

                    let method_obj = Obj::Method(m.clone());
                    global_vars.insert(name, method_obj);
                }
                Value::Slot(val) => {
                    let name = match p.values[val as usize] {
                        Value::Str(ref s) => s.clone(),
                        _ => return Err(io::Error::new(InvalidData, "Invalid object type")),
                    };

                    global_vars.insert(name, Obj::Null);
                }
                _ => return Err(io::Error::new(InvalidData, "Invalid value type")),
            }
        }

        // Preprocess labels
        for val in &p.values {
            if let Value::Method(ref m) = *val {
                for (pc, inst) in m.code.iter().enumerate() {
                    if let Inst::Label(inst) = *inst {
                        let label_str = match p.values[inst as usize] {
                            Value::Str(ref s) => s.clone(),
                            _ => {
                                return Err(io::Error::new(InvalidData,
                                                          "Invalid object type for LABEL"))
                            }
                        };
                        labels.insert(label_str,
                                      LabelAddr {
                                          code: Some(m.code.clone()),
                                          pc: pc as i32,
                                      });
                    }
                }
            }
        }

        Ok(VM {
            global_vars: global_vars,
            labels: labels,
            code: code,
            pc: 0,
            operand: Vec::new(),
            local_frame: local_frame,
        })
    }
}
