use ast::{Exp, ScopeStmt, SlotStmt};
use bytecode::{ClassValue, Program, Inst, Value};
use std::collections::HashMap;
use std::io;

/// Compiles an AST structure into bytecode
pub fn compile(stmt: &ScopeStmt) -> io::Result<Program> {
    use std::io::Error;
    use std::io::ErrorKind::*;

    // TODO(DarinM223): initialize program
    let mut program = Program {
        values: Vec::new(),
        slots: Vec::new(),
        entry: 0,
        null_idx: 0,
    };
    let entry = program.entry as i32;
    program.null_idx = program.add_value(Value::Null) as i16;

    try!(stmt.compile(&mut None, &mut program, entry, &mut HashMap::new()));
    Ok(program)
}

impl Program {
    pub fn add_value(&mut self, value: Value) -> usize {
        self.values.push(value);
        self.values.len() - 1
    }

    pub fn add_instruction(&mut self, index: usize, inst: Inst) -> io::Result<()> {
        if let Some(&mut Value::Method(ref mut method)) = self.values.get_mut(index) {
            method.code.push(inst);
            Ok(())
        } else {
            Err(io::Error::new(io::ErrorKind::InvalidData, "Cannot access method value"))
        }
    }

    pub fn get_str_id(&mut self, name: &str, name_cache: &mut HashMap<String, usize>) -> usize {
        let mut index = 0;
        let mut got_idx = false;
        {
            if let Some(idx) = name_cache.get(name) {
                index = idx.clone();
                got_idx = true;
            }
        }

        if !got_idx {
            index = self.add_value(Value::Str(name.to_owned()));
            name_cache.insert(name.to_owned(), index);
        }

        index
    }
}

impl Exp {
    pub fn compile(&self,
                   env: &mut Option<HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: usize,
                   name_cache: &mut HashMap<String, usize>)
                   -> io::Result<()> {
        let null_idx = program.null_idx;
        match *self {
            Exp::Int(i) => {
                let index = program.add_value(Value::Int(i)) as i16;
                try!(program.add_instruction(method_idx, Inst::Lit(index)));
            }
            Exp::Null => try!(program.add_instruction(method_idx, Inst::Lit(null_idx))),
            Exp::Printf(ref printf) => {
                let format_id = program.get_str_id(&printf.format[..], name_cache);
                for exp in &printf.exps {
                    exp.compile(env, program, method_idx, name_cache);
                }
                try!(program.add_instruction(method_idx,
                                             Inst::Printf(format_id as i16, printf.nexps as u8)));
            }
            Exp::Array(ref arr) => {
                arr.length.compile(env, program, method_idx, name_cache);
                arr.init.compile(env, program, method_idx, name_cache);
                program.add_instruction(method_idx, Inst::Array);
            }
            Exp::Object(ref obj) => {
                obj.parent.compile(env, program, method_idx, name_cache);
                let mut class_val = ClassValue { slots: vec![] };
                for slot in &obj.slots {
                    class_val.slots.push(try!(slot.compile(env, program, method_idx, name_cache)));
                }

                let class_id = program.add_value(Value::Class(class_val));
                try!(program.add_instruction(method_idx, Inst::Object(class_id as i16)));
            }
            Exp::Slot(ref slot) => {
                slot.exp.compile(env, program, method_idx, name_cache);
                let str_id = program.get_str_id(&slot.name[..], name_cache) as i16;
                try!(program.add_instruction(method_idx, Inst::GetSlot(str_id)));
            }
            Exp::SetSlot(ref setslot) => {
                setslot.exp.compile(env, program, method_idx, name_cache);
                setslot.value.compile(env, program, method_idx, name_cache);
                let str_id = program.get_str_id(&setslot.name[..], name_cache) as i16;
                try!(program.add_instruction(method_idx, Inst::SetSlot(str_id)));
                try!(program.add_instruction(method_idx, Inst::Drop));
            }
            Exp::CallSlot(ref callslot) => {}
            Exp::Call(ref call) => {}
            Exp::Set(ref set) => {}
            Exp::If(ref if_exp) => {}
            Exp::While(ref while_exp) => {}
            Exp::Ref(ref ref_exp) => {}
        }
        Ok(())
    }
}

impl ScopeStmt {
    pub fn compile(&self,
                   env: &mut Option<HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: i32,
                   name_cache: &mut HashMap<String, usize>)
                   -> io::Result<()> {
        match *self {
            ScopeStmt::Var(ref var) => {}
            ScopeStmt::Fn(ref fun) => {}
            ScopeStmt::Seq(ref seq) => {}
            ScopeStmt::Exp(ref exp) => {}
        }
        Ok(())
    }
}

impl SlotStmt {
    pub fn compile(&self,
                   env: &mut Option<HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: usize,
                   name_cache: &mut HashMap<String, usize>)
                   -> io::Result<i16> {
        match *self {
            SlotStmt::Var(ref var) => {}
            SlotStmt::Method(ref met) => {}
        }
        Ok(0)
    }
}
