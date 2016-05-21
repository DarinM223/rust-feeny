use ast::{Exp, ScopeStmt, SlotStmt};
use bytecode::{Program, Value};
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
    };
    let entry = program.entry as i32;
    try!(stmt.compile(None, &mut program, entry));
    Ok(program)
}

impl Exp {
    pub fn compile(&self,
                   env: Option<&mut HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: i32)
                   -> io::Result<()> {
        match *self {
            Exp::Int(i) => {}
            Exp::Null => {}
            Exp::Printf(ref printf) => {}
            Exp::Array(ref arr) => {}
            Exp::Object(ref obj) => {}
            Exp::Slot(ref slot) => {}
            Exp::SetSlot(ref setslot) => {}
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
                   env: Option<&mut HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: i32)
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
                   env: Option<&mut HashMap<String, i32>>,
                   program: &mut Program,
                   method_idx: i32)
                   -> io::Result<()> {
        match *self {
            SlotStmt::Var(ref var) => {}
            SlotStmt::Method(ref met) => {}
        }
        Ok(())
    }
}
