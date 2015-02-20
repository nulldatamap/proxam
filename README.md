# Proxam
Proxam is a programming language implementation for my programming exam.
It's written from the bottom up in Rust, and uses rustc's LLVM API for code generation.

More documentation will come later since this is far from finished.
Check `spec.md` for the current specification I'm aiming to implement.

# Status
Currently the following is implemented:
* Function definitions & declarations
* Type system
* Arithmetic operation
* Function calls ( currently neither partial- or over-application is implemented ) 
* If-expressions
* Let-binding

I'm currently working on getting type parameters implement ( generics ) and traits. Which will make compile-time partially applied function passing and closures possible using generics. The type system is bare bones currently since I'm working on language semantics and not completeness. 

Currently correct LLVM-IR is being generated, but optimization passes and native code generation is yet to be moved inside the compiler. 

# How to use
Currently you need to compile the code using the nightly version of Rust and Cargo. The compiler will spit out the llvm IR, which can be compiled to any llvm supported target using `llc`.

# Syntax example:
```
; Naïve Fibonacci implementation

; Defined in rt.c 
def print_int x : Int -> ()

def fib x : Int -> Int = 
  if x < 2
  then 1
  else (fib x - 1) + (fib x - 2)

def main : () =
  let a : Int = 3
  in print_int (fib a * a) 

```
