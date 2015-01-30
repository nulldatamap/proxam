use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef, TypeRef};

use ast::Ident;

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


