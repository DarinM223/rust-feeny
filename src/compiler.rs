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
  let mut name_cache = HashMap::new();
  let entry_str = program.get_str_id("__ENTRY__", &mut name_cache) as i16;
  program.entry = program.add_value(Value::Method(MethodValue {
                                                    name: entry_str,
                                                    nargs: 0,
                                                    nlocals: 0,
                                                    code: Vec::new(),
                                                  })) as i16;
  program.null_idx = program.add_value(Value::Null) as i16;

  let entry = program.entry as usize;
  stmt.compile(&mut None, &mut program, entry, &mut name_cache)?;
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
        program.add_instruction(method_idx, Inst::Lit(index))?;
      }
      Exp::Null => program.add_instruction(method_idx, Inst::Lit(null_idx))?,
      Exp::Printf(ref printf) => {
        debug!("Printf: {} nexps: {}", printf.format, printf.nexps);
        let format_id = program.get_str_id(&printf.format[..], name_cache);
        for exp in &printf.exps {
          exp.compile(env, program, method_idx, name_cache)?;
        }
        program.add_instruction(method_idx, Inst::Printf(format_id as i16, printf.nexps as u8))?;
      }
      Exp::Array(ref arr) => {
        debug!("Array: init: {:?} length: {:?}", arr.init, arr.length);
        arr.length.compile(env, program, method_idx, name_cache)?;
        arr.init.compile(env, program, method_idx, name_cache)?;
        program.add_instruction(method_idx, Inst::Array)?;
      }
      Exp::Object(ref obj) => {
        debug!("Object: parent: {:?} nslots: {}", obj.parent, obj.nslots);
        obj.parent.compile(env, program, method_idx, name_cache)?;
        let mut class_val = ClassValue { slots: vec![] };
        for slot in &obj.slots {
          class_val.slots.push(slot.compile(env, program, method_idx, name_cache)?);
        }

        let class_id = program.add_value(Value::Class(class_val));
        program.add_instruction(method_idx, Inst::Object(class_id as i16))?;
      }
      Exp::Slot(ref slot) => {
        debug!("Slot: name: {} exp: {:?}", slot.name, slot.exp);
        slot.exp.compile(env, program, method_idx, name_cache)?;
        let str_id = program.get_str_id(&slot.name[..], name_cache) as i16;
        program.add_instruction(method_idx, Inst::GetSlot(str_id))?;
      }
      Exp::SetSlot(ref setslot) => {
        debug!("Setting slot: name: {} exp: {:?} value: {:?}",
               setslot.name,
               setslot.exp,
               setslot.value);
        setslot.exp.compile(env, program, method_idx, name_cache)?;
        setslot.value.compile(env, program, method_idx, name_cache)?;
        let str_id = program.get_str_id(&setslot.name[..], name_cache) as i16;
        program.add_instruction(method_idx, Inst::SetSlot(str_id))?;
        program.add_instruction(method_idx, Inst::Drop)?;
      }
      Exp::CallSlot(ref callslot) => {
        debug!("Calling slot: name: {} exp: {:?} args: {:?}",
               callslot.name,
               callslot.exp,
               callslot.args);
        callslot.exp.compile(env, program, method_idx, name_cache)?;
        for arg in &callslot.args {
          arg.compile(env, program, method_idx, name_cache)?;
        }

        let str_id = program.get_str_id(&callslot.name[..], name_cache);
        let nargs = callslot.nargs as u8 + 1;
        program.add_instruction(method_idx, Inst::CallSlot(str_id as i16, nargs))?;
      }
      Exp::Call(ref call) => {
        debug!("Calling: name: {} args: {:?}", call.name, call.args);
        for arg in &call.args {
          arg.compile(env, program, method_idx, name_cache)?;
        }
        let str_id = program.get_str_id(&call.name[..], name_cache) as i16;
        program.add_instruction(method_idx, Inst::Call(str_id, call.nargs as u8))?;
      }
      Exp::Set(ref set) => {
        debug!("Setting: name: {} exp: {:?}", set.name, set.exp);
        set.exp.compile(env, program, method_idx, name_cache)?;
        let inst = match clone_from_opt_map_ref!(env, &set.name) {
          Some(id) => Inst::SetLocal(id as i16),
          None => Inst::SetGlobal(program.get_str_id(&set.name[..], name_cache) as i16),
        };
        program.add_instruction(method_idx, inst)?;
        program.add_instruction(method_idx, Inst::Drop)?;
      }
      Exp::If(ref if_exp) => {
        debug!("If: predicate: {:?} consequence: {:?} alternative: {:?}",
               if_exp.pred,
               if_exp.conseq,
               if_exp.alt);
        let on_true_str = format!("LABEL_{}", program.label_count);
        let on_false_str = format!("LABEL_{}", program.label_count + 1);

        // Add the labels to jump to
        let on_true = program.get_str_id(&on_true_str[..], name_cache);
        let on_false = program.get_str_id(&on_false_str[..], name_cache);
        program.label_count += 2;

        // Compile predicate
        if_exp.pred.compile(env, program, method_idx, name_cache)?;

        // If predicate is true go to on_true:
        program.add_instruction(method_idx, Inst::Branch(on_true as i16))?;

        // Otherwise run instructions in else block then jump to on_false:
        if_exp.alt.compile(env, program, method_idx, name_cache)?;
        program.add_instruction(method_idx, Inst::Goto(on_false as i16))?;

        // on_true: Run instructions in if block
        program.add_instruction(method_idx, Inst::Label(on_true as i16))?;
        if_exp.conseq.compile(env, program, method_idx, name_cache)?;

        // on_false:
        program.add_instruction(method_idx, Inst::Label(on_false as i16))?;
      }
      Exp::While(ref while_exp) => {
        debug!("While: predicate: {:?} body: {:?}", while_exp.pred, while_exp.body);
        let start_str = format!("LABEL_{}", program.label_count);
        let body_str = format!("LABEL_{}", program.label_count + 1);
        let end_str = format!("LABEL_{}", program.label_count + 2);

        // Add the labels to jump to
        let start = program.get_str_id(&start_str[..], name_cache);
        let body = program.get_str_id(&body_str[..], name_cache);
        let end = program.get_str_id(&end_str[..], name_cache);
        program.label_count += 3;

        // start:
        program.add_instruction(method_idx, Inst::Label(start as i16))?;

        // Compile predicate
        while_exp.pred.compile(env, program, method_idx, name_cache)?;

        // If predicate is true go to body:
        program.add_instruction(method_idx, Inst::Branch(body as i16))?;

        // Otherwise go to end:
        program.add_instruction(method_idx, Inst::Goto(end as i16))?;

        // body: evaluate instructions in body then loop to start:
        program.add_instruction(method_idx, Inst::Label(body as i16))?;
        while_exp.body.compile(env, program, method_idx, name_cache)?;
        program.add_instruction(method_idx, Inst::Goto(start as i16))?;

        // end:
        program.add_instruction(method_idx, Inst::Label(end as i16))?;
      }
      Exp::Ref(ref refname) => {
        debug!("Ref: {:?}", refname);
        let inst = match clone_from_opt_map_ref!(env, refname) {
          Some(id) => Inst::GetLocal(id as i16),
          None => Inst::GetGlobal(program.get_str_id(refname, name_cache) as i16),
        };

        program.add_instruction(method_idx, inst)?;
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
        debug!("Var: name: {:?} exp: {:?}", var.name, var.exp);
        if *env != None {
          match *env {
            Some(ref mut env) if env.len() > 1 => {
              let id = *(env.values().max().unwrap()) + 1;
              env.insert(var.name.clone(), id);
            }
            _ => {}
          }
          let mid = method_idx;
          if let Some(&mut Value::Method(ref mut met)) = program.values.get_mut(mid) {
            met.nlocals += 1;
          }
          var.exp.compile(env, program, method_idx, name_cache)?;

          let name_id = program.get_str_id(&var.name[..], name_cache) as i16;
          program.add_instruction(method_idx, Inst::SetLocal(name_id))?;
        } else {
          let name_id = program.get_str_id(&var.name[..], name_cache);
          let slot_id = program.add_value(Value::Slot(name_id as i16));
          program.slots.push(slot_id as i16);
          var.exp.compile(env, program, method_idx, name_cache)?;
          program.add_instruction(method_idx, Inst::SetGlobal(name_id as i16))?;
        }

        program.add_instruction(method_idx, Inst::Drop)?;
      }
      ScopeStmt::Fn(ref fun) => {
        debug!("Scope Fn: name: {:?} args: {:?} body: {:?}", fun.name, fun.args, fun.body);
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
        fun.body.compile(&mut Some(new_env), program, new_method_id, name_cache)?;
        program.slots.push(new_method_id as i16);
        program.add_instruction(new_method_id, Inst::Return)?;
      }
      ScopeStmt::Seq(ref seq) => {
        debug!("Scope Seq: A: {:?} B: {:?}", seq.a, seq.b);
        seq.a.compile(env, program, method_idx, name_cache)?;
        seq.b.compile(env, program, method_idx, name_cache)?;
      }
      ScopeStmt::Exp(ref exp) => {
        debug!("Scope Exp: {:?}", exp);
        exp.compile(env, program, method_idx, name_cache)?;
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
        debug!("Slot var: name: {:?} exp: {:?}", var.name, var.exp);
        let slot_name = program.get_str_id(&var.name[..], name_cache);
        let slot_id = program.add_value(Value::Slot(slot_name as i16));
        var.exp.compile(env, program, method_idx, name_cache)?;
        Ok(slot_id as i16)
      }
      SlotStmt::Method(ref met) => {
        debug!("Slot method: name: {:?} args: {:?} body: {:?}", met.name, met.args, met.body);
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

        met.body.compile(&mut Some(new_env), program, new_method_id, name_cache)?;
        program.add_instruction(new_method_id, Inst::Return)?;
        Ok(new_method_id as i16)
      }
    }
  }
}
