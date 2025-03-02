/// Retrieves a string from the program values given an index or returns an Err
#[macro_export]
macro_rules! get_str_val {
    ($index:expr, $program:expr, $inst:expr) => {
        match $program.values.get($index as usize) {
            Some(Value::Str(s)) => s.clone(),
            _ => {
                return Err(io::Error::new(
                    InvalidData,
                    format!("{:?}: Invalid index when retrieving string", $inst),
                ))
            }
        }
    };
}

/// Clones out a value from a reference to a optional containing
/// a HashMap and returns an Option with the cloned value
#[macro_export]
macro_rules! clone_from_opt_map_ref {
    ($opt_env:expr, $key:expr) => {
        match *$opt_env {
            Some(ref env) => env.get($key).cloned(),
            None => None,
        }
    };
}
