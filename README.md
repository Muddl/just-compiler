Just Compiler
=====
A Julia Compiler built in Rust
-----

Heavily inspired by [first-step-rust](https://github.com/pku-minic/first-step-rust)
Ported over by Wade Kaiser for CS4308 Fall '21

Please note that this project is still incomplete.
Parsing is 90% complete but programs are unfortunately not runnable.

Before running the project, don't forget to `cargo build --release`

To interpret a file, run `cargo run --release -- {FILE-PATH-HERE}`

Once parsing is complete, files may be compiled to RISC-V assembly executables using `cargo run --release -- {FILE-PATH=HERE} -c -o {OUTPUT-FILE-HERE}`

**I am exercising my P2 push to 11/14 for 10%.**