use bytecode::{Inst, MethodValue, Program, Value};
use interpreter::{EnvObj, EnvObjRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::mem;
use std::rc::Rc;

pub type Code = Rc<RefCell<Vec<Inst>>>;

/// Interprets the bytecode structure
pub fn interpret_bc(program: Program) -> io::Result<()> {
    use std::io::ErrorKind::*;

    let mut vm = try!(VM::new(&program));

    while vm.pc < vm.code.len() as i32 {
        let inst = vm.code.get(vm.pc as usize).unwrap();

        // TODO(DarinM223): implement this
        match *inst {
            Inst::Lit(idx) => {
                // Retrieve object from index in the constant pool
                // and push into the operand stack
                match program.values.get(idx as usize) {
                    Some(&Value::Int(i)) => vm.operand.push(Obj::Int(i)),
                    Some(&Value::Null) => vm.operand.push(Obj::Null),
                    _ => return Err(io::Error::new(InvalidData, "Invalid object type for LIT")),
                }
            }
            Inst::Array => {
                // Pop the initializing value from the operand stack,
                // pop the the length of the array from the operand stack,
                // then push the array onto the operand stack
                if let (Some(init), Some(Obj::Int(len))) = (vm.operand.pop(), vm.operand.pop()) {
                    vm.operand.push(Obj::Array(vec![init; len as usize]));
                } else {
                    return Err(io::Error::new(InvalidData, "Invalid length type for ARRAY"));
                }
            }
            Inst::Printf(format, arity) => {}
            Inst::Object(class) => {}
            Inst::GetSlot(name) => {}
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

/// An object that can be stored as a variable
/// used in the global variable constant pool,
/// the operand stack, and the frames
#[derive(Clone, Debug, PartialEq)]
pub enum Obj {
    Int(i32),
    Null,
    Array(Vec<Obj>),
    Method(MethodValue),
    EnvObj(EnvObjRef<Obj>),
}

/// Contains a reference counted pointer to the parent method's code
/// and the program counter of the label
#[derive(Clone, Debug)]
struct LabelAddr {
    code: Option<Code>,
    pc: i32,
}

/// The context in which a function is executing
#[derive(Clone, Debug)]
pub struct Frame {
    /// Arguments to the function + local variables
    slots: Vec<Obj>,
    /// The address of the instruction that called the function
    ret_addr: LabelAddr,
    /// The context of the parent
    parent: Option<Box<Frame>>,
}

impl Frame {
    pub fn new(code: Option<Code>, pc: i32, num_slots: i32, parent: Option<Box<Frame>>) -> Frame {
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
    /// Global variable and label name-to-value maps
    global_vars: HashMap<String, Obj>,
    labels: HashMap<String, LabelAddr>,

    code: Vec<Inst>,
    /// Address of the next instruction to execute
    pc: i32,
    /// A stack for the temp results for evaluation
    operand: Vec<Obj>,
    /// The context in which the current frame is executing
    local_frame: Frame,
}

impl VM {
    pub fn new(p: &Program) -> io::Result<VM> {
        use std::io::ErrorKind::*;

        let mut global_vars = HashMap::with_capacity(VM_CAPACITY);
        let mut labels = HashMap::with_capacity(VM_CAPACITY);
        let local_frame;
        let code;

        if let Some(&Value::Method(ref entry_func)) = p.values.get(p.entry as usize) {
            local_frame = Frame::new(None,
                                     0,
                                     (entry_func.nargs as i32) + (entry_func.nlocals as i32),
                                     None);
            code = entry_func.code.clone();
        } else {
            return Err(io::Error::new(InvalidInput, "Entry function needs to be a method value!"));
        }

        // Initialize global variable constant pool
        for idx in &p.slots {
            let value = p.values.get(*idx as usize).unwrap();
            match *value {
                Value::Method(ref m) => {
                    // retrieve the method name from the constant pool
                    let name = match p.values[m.name as usize] {
                        Value::Str(ref s) => s.clone(),
                        _ => return Err(io::Error::new(InvalidData, "Invalid object type")),
                    };

                    global_vars.insert(name, Obj::Method(m.clone()));
                }
                Value::Slot(val) => {
                    // retrieve the slot name from the constant pool
                    let name = match p.values[val as usize] {
                        Value::Str(ref s) => s.clone(),
                        _ => return Err(io::Error::new(InvalidData, "Invalid object type")),
                    };

                    global_vars.insert(name, Obj::Null);
                }
                _ => return Err(io::Error::new(InvalidData, "Invalid value type")),
            }
        }

        // Store labels in map
        for val in &p.values {
            if let Value::Method(ref m) = *val {
                let code = Rc::new(RefCell::new(m.code.clone()));

                for (pc, inst) in m.code.iter().enumerate() {
                    if let Inst::Label(inst) = *inst {
                        // Retrieve the label name from the constant pool
                        let label_str = match p.values[inst as usize] {
                            Value::Str(ref s) => s.clone(),
                            _ => {
                                return Err(io::Error::new(InvalidData,
                                                          "Invalid object type for LABEL"))
                            }
                        };

                        // Insert into the map with the key as the label name
                        // and the label containing the program counter at the label
                        // and the code being the cloned code of the method
                        // TODO(DarinM223): does all code fields have to point back to the same
                        // data or can it just be cloned every time?
                        labels.insert(label_str,
                                      LabelAddr {
                                          code: Some(code.clone()), // increment reference count
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
