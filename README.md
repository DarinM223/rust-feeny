rust-feeny
==========

Learning virtual machines, garbage collectors, and bytecode interpreters by working through the homework for the UC Berkeley graduate course UCB CS294: Virtual Machines and Managed Runtimes

Running:
--------

First you have to run cargo to build the project:
```
cargo build
```

Note: in order to run the sudoku.ast and sudoku2.ast test files it is better to
build the project in release mode for better performance
```
cargo build --release
```

To run the ast interpreter, you have to specify an AST file.
To print debug statments use RUST_LOG=debug otherwise use RUST_LOG=info
```
RUST_LOG=info ./target/debug/feeny ast output/hello.ast
RUST_LOG=debug ./target/debug/feeny ast output/hello.ast
```

To run the bytecode interpreter, you have to specify a bytecode file.
```
RUST_LOG=info ./target/debug/feeny bc output/hello.bc
RUST_LOG=debug ./target/debug/feeny bc output/hello.bc
```

For release mode:
```
./target/release/feeny ./output/sudoku.ast
```
