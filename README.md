rust-feeny
==========

Learning virtual machines, garbage collectors, and bytecode interpreters by working through the homework for the UC Berkeley graduate course UCB CS294: Virtual Machines and Managed Runtimes

Running:
--------

First you have to run cargo to build the project:
```
cargo build
```

Then to run the interpreter, you have to specify an AST file.
To print debug statments use RUST_LOG=debug otherwise use RUST_LOG=info
```
RUST_LOG=info ./target/debug/feeny output/hello.ast
RUST_LOG=debug ./target/debug/feeny output/hello.ast
```
