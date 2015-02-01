use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef, TypeRef, BuilderRef};

use ast::Ident;
use codegen::noname;

#[derive(Debug, Clone, PartialEq)]
pub enum BuiltinType {
  Int,
  Bool
}

impl BuiltinType {
  pub fn as_llvm_type( &self, ctx : ContextRef ) -> TypeRef {
    unsafe {
      match self {
        &BuiltinType::Int => llvm::LLVMInt32TypeInContext( ctx ),
        &BuiltinType::Bool => llvm::LLVMInt1TypeInContext( ctx )
      }
    }
  }
}

pub fn builtin_type( tn : &Ident ) -> Option<BuiltinType> {
  Some( match &tn.text[] {
    "Int" => BuiltinType::Int,
    "Bool" => BuiltinType::Bool,
    _ => return None
  } )
}

#[derive(Show)]
pub enum BuiltinFn {
  Add,
  Cmp( u32 )
}

impl BuiltinFn {
  pub fn as_llvm_value( &self, bldr : BuilderRef, args : &[ValueRef] )
             -> ValueRef {
    unsafe {
      match args {
        [ a, b ] =>
          match self {
            &BuiltinFn::Add => return llvm::LLVMBuildAdd( bldr, a, b, noname() ),
            &BuiltinFn::Cmp( op ) => return llvm::LLVMBuildICmp( bldr
                                                               , op
                                                               , a, b
                                                               , noname() ),
          },
        _ => {}
      }
    }
    panic!( "Malformed builtin function call curing codegen: {:?} - {:?}"
          , self, args )
  }
}

pub fn builtin_fn( name : &Ident ) -> Option<BuiltinFn> {
  Some( match &name.text[] {
    "+" => BuiltinFn::Add,
    "==" => BuiltinFn::Cmp( llvm::IntPredicate::IntEQ as u32 ),
    _ => return None
  } )
}

