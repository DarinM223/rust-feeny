use ast::{Exp, ScopeStmt, SlotStmt};
use bytecode::{ClassValue, MethodValue, Program, Inst, Value};
use std::collections::HashMap;
use std::io;

/// Compiles an AST structure into bytecode
pub fn compile(stmt: &ScopeStmt) -> io::Result<Program> {
    let mut program = Program {
        values: Vec::new(),
        slots: Vec::new(),
        entry: 2,
        null_idx: 0,
        label_count: 0,
    };
    let entry = program.entry as usize;
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
                    try!(exp.compile(env, program, method_idx, name_cache));
                }
                try!(program.add_instruction(method_idx,
                                             Inst::Printf(format_id as i16, printf.nexps as u8)));
            }
            Exp::Array(ref arr) => {
                try!(arr.length.compile(env, program, method_idx, name_cache));
                try!(arr.init.compile(env, program, method_idx, name_cache));
                try!(program.add_instruction(method_idx, Inst::Array));
            }
            Exp::Object(ref obj) => {
                try!(obj.parent.compile(env, program, method_idx, name_cache));
                let mut class_val = ClassValue { slots: vec![] };
                for slot in &obj.slots {
                    class_val.slots.push(try!(slot.compile(env, program, method_idx, name_cache)));
                }

                let class_id = program.add_value(Value::Class(class_val));
                try!(program.add_instruction(method_idx, Inst::Object(class_id as i16)));
            }
            Exp::Slot(ref slot) => {
                try!(slot.exp.compile(env, program, method_idx, name_cache));
                let str_id = program.get_str_id(&slot.name[..], name_cache) as i16;
                try!(program.add_instruction(method_idx, Inst::GetSlot(str_id)));
            }
            Exp::SetSlot(ref setslot) => {
                try!(setslot.exp.compile(env, program, method_idx, name_cache));
                try!(setslot.value.compile(env, program, method_idx, name_cache));
                let str_id = program.get_str_id(&setslot.name[..], name_cache) as i16;
                try!(program.add_instruction(method_idx, Inst::SetSlot(str_id)));
                try!(program.add_instruction(method_idx, Inst::Drop));
            }
            Exp::CallSlot(ref callslot) => {
                try!(callslot.exp.compile(env, program, method_idx, name_cache));
                for arg in &callslot.args {
                    try!(arg.compile(env, program, method_idx, name_cache));
                }

                let str_id = program.get_str_id(&callslot.name[..], name_cache);
                let nargs = callslot.nargs as u8 + 1;
                try!(program.add_instruction(method_idx, Inst::CallSlot(str_id as i16, nargs)));
            }
            Exp::Call(ref call) => {
                for arg in &call.args {
                    try!(arg.compile(env, program, method_idx, name_cache));
                }
                let str_id = program.get_str_id(&call.name[..], name_cache) as i16;
                try!(program.add_instruction(method_idx, Inst::Call(str_id, call.nargs as u8)));
            }
            Exp::Set(ref set) => {
                try!(set.exp.compile(env, program, method_idx, name_cache));
                let inst = match clone_from_opt_map_ref!(env, &set.name) {
                    Some(id) => Inst::SetLocal(id as i16),
                    None => Inst::SetGlobal(program.get_str_id(&set.name[..], name_cache) as i16),
                };
                try!(program.add_instruction(method_idx, inst));
                try!(program.add_instruction(method_idx, Inst::Drop));
            }
            Exp::If(ref if_exp) => {
                let on_true_str = format!("LABEL_{}", program.label_count);
                let on_false_str = format!("LABEL_{}", program.label_count + 1);

                // Add the labels to jump to
                let on_true = program.get_str_id(&on_true_str[..], name_cache);
                let on_false = program.get_str_id(&on_false_str[..], name_cache);
                program.label_count += 2;

                // Compile predicate
                try!(if_exp.pred.compile(env, program, method_idx, name_cache));

                // If predicate is true go to on_true:
                try!(program.add_instruction(method_idx, Inst::Branch(on_true as i16)));

                // Otherwise run instructions in else block then jump to on_false:
                try!(if_exp.alt.compile(env, program, method_idx, name_cache));
                try!(program.add_instruction(method_idx, Inst::Goto(on_false as i16)));

                // on_true: Run instructions in if block
                try!(program.add_instruction(method_idx, Inst::Label(on_true as i16)));
                try!(if_exp.conseq.compile(env, program, method_idx, name_cache));

                // on_false:
                try!(program.add_instruction(method_idx, Inst::Label(on_false as i16)));
            }
            Exp::While(ref while_exp) => {
                let start_str = format!("LABEL_{}", program.label_count);
                let body_str = format!("LABEL_{}", program.label_count + 1);
                let end_str = format!("LABEL_{}", program.label_count + 2);

                // Add the labels to jump to
                let start = program.get_str_id(&start_str[..], name_cache);
                let body = program.get_str_id(&body_str[..], name_cache);
                let end = program.get_str_id(&end_str[..], name_cache);
                program.label_count += 3;

                // start:
                try!(program.add_instruction(method_idx, Inst::Label(start as i16)));

                // Compile predicate
                try!(while_exp.pred.compile(env, program, method_idx, name_cache));

                // If predicate is true go to body:
                try!(program.add_instruction(method_idx, Inst::Branch(body as i16)));

                // Otherwise go to end:
                try!(program.add_instruction(method_idx, Inst::Goto(end as i16)));

                // body: evaluate instructions in body then loop to start:
                try!(program.add_instruction(method_idx, Inst::Label(body as i16)));
                try!(while_exp.body.compile(env, program, method_idx, name_cache));
                try!(program.add_instruction(method_idx, Inst::Goto(start as i16)));

                // end:
                try!(program.add_instruction(method_idx, Inst::Label(end as i16)));
            }
            Exp::Ref(ref refname) => {
                let inst = match clone_from_opt_map_ref!(env, refname) {
                    Some(id) => Inst::GetLocal(id as i16),
                    None => Inst::GetGlobal(program.get_str_id(refname, name_cache) as i16),
                };

                try!(program.add_instruction(method_idx, inst));
            }
        }
        Ok(())
    }
}

impl ScopeStmt {
    pub fn compile(&self,
                   env: &mut Option<HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: usize,
                   name_cache: &mut HashMap<String, usize>)
                   -> io::Result<()> {
        match *self {
            ScopeStmt::Var(ref var) => {
                if *env != None {
                    if let Some(ref mut env) = *env {
                        let id = *(env.values().max().unwrap()) + 1;
                        env.insert(var.name.clone(), id);
                    }
                    let mid = method_idx;
                    if let Some(&mut Value::Method(ref mut met)) = program.values.get_mut(mid) {
                        met.nlocals += 1;
                    }
                    try!(var.exp.compile(env, program, method_idx, name_cache));

                    let name_id = program.get_str_id(&var.name[..], name_cache) as i16;
                    try!(program.add_instruction(method_idx, Inst::SetLocal(name_id)));
                } else {
                    let name_id = program.get_str_id(&var.name[..], name_cache);
                    let slot_id = program.add_value(Value::Slot(name_id as i16));
                    program.slots.push(slot_id as i16);
                    try!(var.exp.compile(env, program, method_idx, name_cache));
                    try!(program.add_instruction(method_idx, Inst::SetGlobal(name_id as i16)));
                }

                try!(program.add_instruction(method_idx, Inst::Drop));
            }
            ScopeStmt::Fn(ref fun) => {
                let name_id = program.get_str_id(&fun.name[..], name_cache) as i16;
                let new_method_id = program.add_value(Value::Method(MethodValue {
                    code: Vec::new(),
                    name: name_id,
                    nargs: fun.nargs as u8,
                    nlocals: 0,
                }));
                let mut new_env = HashMap::new();
                for (i, arg) in fun.args.iter().enumerate() {
                    new_env.insert(arg.clone(), i as i32);
                }
                try!(fun.body.compile(&mut Some(new_env), program, new_method_id, name_cache));
                program.slots.push(new_method_id as i16);
                try!(program.add_instruction(new_method_id, Inst::Return));
            }
            ScopeStmt::Seq(ref seq) => {
                try!(seq.a.compile(env, program, method_idx, name_cache));
                try!(seq.b.compile(env, program, method_idx, name_cache));
            }
            ScopeStmt::Exp(ref exp) => {
                try!(exp.compile(env, program, method_idx, name_cache));
            }
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
            SlotStmt::Var(ref var) => {
                let slot_name = program.get_str_id(&var.name[..], name_cache);
                let slot_id = program.add_value(Value::Slot(slot_name as i16));
                try!(var.exp.compile(env, program, method_idx, name_cache));
                Ok(slot_id as i16)
            }
            SlotStmt::Method(ref met) => {
                let name = program.get_str_id(&met.name[..], name_cache) as i16;
                let new_method_id = program.add_value(Value::Method(MethodValue {
                    code: Vec::new(),
                    name: name,
                    nargs: met.nargs as u8,
                    nlocals: 0,
                }));

                let mut new_env = HashMap::new();
                new_env.insert("this".to_owned(), 0);
                for (i, arg) in met.args.iter().enumerate() {
                    new_env.insert(arg.clone(), (i + 1) as i32);
                }

                try!(met.body.compile(&mut Some(new_env), program, new_method_id, name_cache));
                try!(program.add_instruction(new_method_id, Inst::Return));
                Ok(new_method_id as i16)
            }
        }
    }
}
