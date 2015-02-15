# Proxam
Proxam is a programming language implementation for my programming exam.
It's written from the bottom up in Rust, and uses rustc's LLVM API for code generation.

More documentation will come later since this is far from finished.
Check `spec.md` for the current specification I'm aiming to implement.

# How to use
Currently you need to compile the code using the nightly version of Rust and Cargo. The compiler will spit out the llvm IR, which can be compiled to any llvm supported target using `llc`.
