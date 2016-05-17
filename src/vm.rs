use bytecode::{Inst, MethodValue, Program, Value};
use interpreter::{EnvObj, EnvObjRef};
use std::cell::RefCell;
use std::collections::HashMap;
use std::io;
use std::mem;
use std::rc::Rc;

/// Similar to unwrap() but doesn't move value and resolves to a reference
macro_rules! get_ref {
    ($data:expr) => (match $data {
        Some(ref val) => val,
        None => panic!("Optional cannot be None"),
    });
}

/// Similar to unwrap() but doesn't move value and resolves to a mutable reference
macro_rules! get_mut_ref {
    ($data:expr) => (match $data {
        Some(ref mut val) => val,
        None => panic!("Optional cannot be None"),
    });
}

/// Retrieves a string from the program values given an index or returns an Err
macro_rules! get_str_val {
    ($index:expr, $program:expr, $inst:expr) => (match $program.values.get($index as usize) {
        Some(&Value::Str(ref s)) => s.clone(),
        _ => return Err(io::Error::new(InvalidData,
                                       format!("{:?}: Invalid index when retrieving string",
                                               $inst))),
    });
}

/// Interprets the bytecode structure
pub fn interpret_bc(program: Program) -> io::Result<()> {
    use std::io::Error;
    use std::io::ErrorKind::*;

    let mut vm = try!(VM::new(&program));

    while vm.pc < vm.code.len() as i32 {
        match *vm.code.get(vm.pc as usize).unwrap() {
            Inst::Lit(idx) => {
                match program.values.get(idx as usize) {
                    Some(&Value::Int(i)) => vm.operand.push(Obj::Int(i)),
                    Some(&Value::Null) => vm.operand.push(Obj::Null),
                    _ => return Err(Error::new(InvalidData, "Invalid object type for LIT")),
                }
            }
            Inst::Array => {
                if let (Some(init), Some(Obj::Int(len))) = (vm.operand.pop(), vm.operand.pop()) {
                    vm.operand.push(Obj::Array(vec![init; len as usize]));
                } else {
                    return Err(Error::new(InvalidData, "Invalid length type for ARRAY"));
                }
            }
            Inst::Printf(format, num) => {
                {
                    let mut args = (0..num).map(|_| vm.operand.pop()).flat_map(|v| v).rev();
                    let format_str = get_str_val!(format, program, "Printf");

                    // Print the values from last popped to first popped
                    for ch in format_str.chars() {
                        if ch == '~' {
                            if let Some(Obj::Int(i)) = args.next() {
                                print!("{}", i);
                            } else {
                                return Err(Error::new(InvalidInput, "Printf: Error printing int"));
                            }
                        } else {
                            print!("{}", ch);
                        }
                    }
                }
                vm.operand.push(Obj::Null);
            }
            Inst::Object(class) => {
                if let Some(&Value::Class(ref classvalue)) = program.values.get(class as usize) {
                    let operand = &mut vm.operand;
                    // For all of the slots in the class value,
                    // pop a value from the operand stack into a list
                    // from lastly-popped to firstly-popped
                    let args: Vec<Obj> = classvalue.slots
                        .iter()
                        .filter_map(|&slot| match program.values.get(slot as usize) {
                            Some(&Value::Slot(_)) => operand.pop(),
                            _ => None,
                        })
                        .rev()
                        .collect();
                    let mut args = args.into_iter();
                    let mut obj = if let Some(Obj::EnvObj(parent)) = operand.pop() {
                        EnvObj::new(Some(parent))
                    } else {
                        return Err(Error::new(InvalidData, "Object: Parent not object type"));
                    };

                    for slot in &classvalue.slots {
                        match program.values.get(*slot as usize) {
                            Some(&Value::Method(ref m)) => {
                                let name = get_str_val!(m.name, program, "Object");
                                obj.add(&name[..], Obj::Method(m.clone()));
                            }
                            Some(&Value::Slot(name)) => {
                                let name = get_str_val!(name, program, "Object");
                                obj.add(&name[..], args.next().unwrap());
                            }
                            _ => {}
                        }
                    }

                    operand.push(Obj::EnvObj(Rc::new(RefCell::new(obj))));
                } else {
                    return Err(Error::new(InvalidInput, "Object: Invalid index"));
                }
            }
            Inst::GetSlot(name) => {
                let name = get_str_val!(name, program, "GetSlot");
                if let Some(Obj::EnvObj(ref obj)) = vm.operand.pop() {
                    obj.borrow().get(&name[..]).map(|val| vm.operand.push(val));
                } else {
                    return Err(Error::new(InvalidData, "GetSlot: Not object type"));
                }
            }
            Inst::SetSlot(name) => {
                let name = get_str_val!(name, program, "SetSlot");
                if let (Some(value), Some(Obj::EnvObj(obj))) = (vm.operand.pop(),
                                                                vm.operand.pop()) {
                    obj.borrow_mut().add(&name[..], value.clone());
                    vm.operand.push(value);
                } else {
                    return Err(Error::new(InvalidData, "SetSlot: Not object type"));
                }
            }
            Inst::CallSlot(name, num) => {
                let operand = &mut vm.operand;
                let mut args: Vec<_> =
                    (0..num - 1).map(|_| operand.pop()).flat_map(|v| v).rev().collect();
                let name = get_str_val!(name, program, "CallSlot");

                match operand.pop() {
                    Some(Obj::Int(i)) => {
                        if num != 2 {
                            return Err(Error::new(InvalidData, "CallSlot: Int arity must be 2"));
                        }

                        let arg = match args[0] {
                            Obj::Int(i) => i,
                            _ => return Err(Error::new(InvalidData, "CallSlot: arg must be Int")),
                        };

                        operand.push(match &name[..] {
                            "add" => Obj::Int(i + arg),
                            "sub" => Obj::Int(i - arg),
                            "mul" => Obj::Int(i * arg),
                            "div" => Obj::Int(i / arg),
                            "mod" => Obj::Int(i % arg),
                            "lt" => Obj::from_bool(i < arg),
                            "gt" => Obj::from_bool(i > arg),
                            "le" => Obj::from_bool(i <= arg),
                            "ge" => Obj::from_bool(i >= arg),
                            "eq" => Obj::from_bool(i == arg),
                            _ => return Err(Error::new(InvalidData, "CallSlot: Invalid operator")),
                        });
                    }
                    Some(Obj::Array(mut arr)) => {
                        match &name[..] {
                            "length" => operand.push(Obj::Int(arr.len() as i32)),
                            "set" => {
                                if num != 3 {
                                    return Err(Error::new(InvalidData,
                                                          "CallSlot: Arr set arity must be 3"));
                                }
                                let (data, index) = (args.pop().unwrap(), args.pop().unwrap());
                                let index = match index {
                                    Obj::Int(i) => i as usize,
                                    _ => {
                                        return Err(Error::new(InvalidData,
                                                              "CallSlot: set index not int"));
                                    }
                                };
                                arr[index] = data;
                                operand.push(Obj::Null);
                            }
                            "get" => {
                                if num != 2 {
                                    return Err(Error::new(InvalidData,
                                                          "CallSlot: Arr get arity must be 2"));
                                }
                                operand.push(args.remove(0));
                            }
                            _ => return Err(Error::new(InvalidData, "CallSlot: Invalid name")),
                        }
                    }
                    Some(Obj::EnvObj(obj)) => {
                        let (code, nargs) = match obj.borrow().get(&name[..]) {
                            Some(Obj::Method(m)) => {
                                (m.code.clone(), m.nargs as usize + m.nlocals as usize + 1)
                            }
                            _ => return Err(Error::new(InvalidData, "CallSlot: Invalid name")),
                        };

                        let mut slots = vec![Obj::Null; nargs];
                        // Slot 0 in the new local frame holds the receiver object
                        // Following slots hold argument values from last-popped to first-popped
                        slots[0] = Obj::EnvObj(obj);
                        args.into_iter().fold(1, |index, arg| {
                            slots[index] = arg;
                            index + 1
                        });

                        let new_frame = Frame {
                            slots: slots,
                            ret_addr: LabelAddr {
                                code: Some(mem::replace(&mut vm.code, code)),
                                pc: vm.pc,
                            },
                            parent: Some(Box::new(vm.local_frame.take().unwrap())),
                        };

                        vm.local_frame = Some(new_frame);
                        vm.pc = -1;
                    }
                    _ => return Err(Error::new(InvalidData, "Unknown object type")),
                }
            }
            Inst::SetLocal(idx) => {
                let value = match vm.operand.last() {
                    Some(v) => v.clone(),
                    None => return Err(Error::new(InvalidData, "SetLocal: Op stack empty")),
                };
                get_mut_ref!(vm.local_frame).slots[idx as usize] = value;
            }
            Inst::GetLocal(idx) => {
                let value = match get_ref!(vm.local_frame).slots.get(idx as usize) {
                    Some(v) => v.clone(),
                    _ => return Err(Error::new(InvalidInput, "GetLocal: Invalid index")),
                };
                vm.operand.push(value);
            }
            Inst::SetGlobal(name) => {
                let name = get_str_val!(name, program, "SetGlobal");
                let value = match vm.operand.last() {
                    Some(v) => v.clone(),
                    None => return Err(Error::new(InvalidData, "SetGlobal: Op Stack empty")),
                };

                vm.global_vars.insert(name, value);
            }
            Inst::GetGlobal(name) => {
                let name = get_str_val!(name, program, "GetGlobal");
                let value = match vm.global_vars.get(&name) {
                    Some(v) => v.clone(),
                    None => return Err(Error::new(InvalidData, "GetGlobal: Invalid name")),
                };

                vm.operand.push(value);
            }
            Inst::Drop => {
                let _ = vm.operand.pop();
            }
            Inst::Label(..) => {}
            Inst::Branch(name) => {
                let name = get_str_val!(name, program, "Branch");
                let pc = match vm.labels.get(&name) {
                    Some(ref label) => label.pc,
                    _ => return Err(Error::new(InvalidData, "Branch: Invalid name")),
                };

                match vm.operand.pop() {
                    Some(Obj::Null) => return Err(Error::new(InvalidData, "Branch: Type is Null")),
                    Some(_) => vm.pc = pc,
                    None => return Err(Error::new(InvalidInput, "Branch: Invalid index")),
                }
            }
            Inst::Goto(name) => {
                let name = get_str_val!(name, program, "Goto");
                let pc = match vm.labels.get(&name) {
                    Some(ref label) => label.pc,
                    _ => return Err(Error::new(InvalidData, "Goto: Invalid name")),
                };

                vm.pc = pc;
            }
            Inst::Call(name, num) => {
                let operand = &mut vm.operand;
                let args = (0..num).map(|_| operand.pop()).flat_map(|v| v).rev();
                let name = get_str_val!(name, program, "Call");
                let (code, nslots) = if let Some(&Obj::Method(ref m)) = vm.global_vars.get(&name) {
                    (m.code.clone(), m.nargs as usize + m.nlocals as usize)
                } else {
                    return Err(Error::new(InvalidData, "Call: Invalid method type"));
                };

                let mut slots = vec![Obj::Null; nslots];
                // Populate slots with the argument values from
                // last popped to first popped from the operand stack
                args.fold(0, |counter, arg| {
                    slots[counter] = arg;
                    counter + 1
                });

                let new_frame = Frame {
                    slots: slots,
                    ret_addr: LabelAddr {
                        code: Some(mem::replace(&mut vm.code, code)),
                        pc: vm.pc,
                    },
                    parent: Some(Box::new(vm.local_frame.take().unwrap())),
                };
                vm.local_frame = Some(new_frame);
                vm.pc = -1;
            }
            Inst::Return => {
                vm.local_frame.take().map(|mut frame| {
                    if let Some(parent) = frame.parent.take() {
                        vm.local_frame = Some(*parent);
                    }
                    vm.pc = frame.ret_addr.pc;
                    vm.code = frame.ret_addr.code.take().unwrap();
                });

                // Return once all of the frames have been returned from
                if vm.local_frame == None {
                    break;
                }
            }
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

impl Obj {
    /// Creates an Integer object if true, otherwise creates a Null object
    pub fn from_bool(b: bool) -> Obj {
        match b {
            true => Obj::Int(0),
            false => Obj::Null,
        }
    }
}

/// Contains the parent method's code
/// and the program counter of the label
#[derive(Clone, Debug, PartialEq)]
struct LabelAddr {
    code: Option<Vec<Inst>>,
    pc: i32,
}

/// The context in which a function is executing
#[derive(Clone, Debug, PartialEq)]
pub struct Frame {
    /// Arguments to the function + local variables
    slots: Vec<Obj>,
    /// The address of the instruction that called the function
    ret_addr: LabelAddr,
    /// The context of the parent
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
    // Global variable and label name-to-value maps
    global_vars: HashMap<String, Obj>,
    labels: HashMap<String, LabelAddr>,

    code: Vec<Inst>,
    /// Address of the next instruction to execute
    pc: i32,
    /// A stack for the temp results for evaluation
    operand: Vec<Obj>,
    /// The context in which the current frame is executing
    local_frame: Option<Frame>,
}

impl VM {
    pub fn new(p: &Program) -> io::Result<VM> {
        use std::io::Error;
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
            return Err(Error::new(InvalidInput, "Entry function needs to be a method value!"));
        }

        // Initialize global variable constant pool
        for idx in &p.slots {
            match p.values.get(*idx as usize) {
                Some(&Value::Method(ref m)) => {
                    // retrieve the method name from the constant pool
                    let name = get_str_val!(m.name, p, "VM_Method");
                    global_vars.insert(name, Obj::Method(m.clone()));
                }
                Some(&Value::Slot(val)) => {
                    // retrieve the slot name from the constant pool
                    let name = get_str_val!(val, p, "VM_Slot");
                    global_vars.insert(name, Obj::Null);
                }
                _ => return Err(Error::new(InvalidData, "Invalid value type")),
            }
        }

        // Store labels in map
        for val in &p.values {
            if let Value::Method(ref m) = *val {
                for (pc, inst) in m.code.iter().enumerate() {
                    if let Inst::Label(inst) = *inst {
                        // Retrieve the label name from the constant pool
                        let label_str = get_str_val!(inst, p, "VM_Label");

                        // Insert into the map with the key as the label name
                        // and the label containing the program counter at the label
                        // and the code being the cloned code of the method
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
            local_frame: Some(local_frame),
        })
    }
}
