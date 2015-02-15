use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef, TypeRef, BuilderRef};

use ast::{Ident, Type};
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

#[derive(Debug, Clone)]
pub enum BuiltinFn {
  Add, Sub, Div, Mul, Mod,
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
            &BuiltinFn::Sub => return llvm::LLVMBuildSub( bldr, a, b, noname() ),
            &BuiltinFn::Div => return llvm::LLVMBuildSDiv( bldr, a, b, noname() ),
            &BuiltinFn::Mul => return llvm::LLVMBuildMul( bldr, a, b, noname() ),
            &BuiltinFn::Mod => return llvm::LLVMBuildSRem( bldr, a, b, noname() ),
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

  // TODO: Make these types more generic
  pub fn get_type( &self ) -> Type {
    match self {
      &BuiltinFn::Add
      | &BuiltinFn::Sub
      | &BuiltinFn::Div
      | &BuiltinFn::Mul
      | &BuiltinFn::Mod =>
        Type::Fn( vec![ Type::BuiltinType( BuiltinType::Int )
                      , Type::BuiltinType( BuiltinType::Int ) ]
                , Box::new( Type::BuiltinType( BuiltinType::Int ) ) ),
      &BuiltinFn::Cmp( .. ) =>
        Type::Fn( vec![ Type::BuiltinType( BuiltinType::Int )
                      , Type::BuiltinType( BuiltinType::Int ) ]
                , Box::new( Type::BuiltinType( BuiltinType::Bool ) ) )
    }
  }
}

pub fn builtin_fn( name : &Ident ) -> Option<BuiltinFn> {
  Some( match &name.text[] {
    "+" => BuiltinFn::Add,
    "-" => BuiltinFn::Sub,
    "/" => BuiltinFn::Div,
    "*" => BuiltinFn::Mul,
    "mod" => BuiltinFn::Mod,
    "==" => BuiltinFn::Cmp( llvm::IntPredicate::IntEQ as u32 ),
    "!=" => BuiltinFn::Cmp( llvm::IntPredicate::IntNE as u32 ),
    "<" => BuiltinFn::Cmp( llvm::IntPredicate::IntSLT as u32 ),
    ">" => BuiltinFn::Cmp( llvm::IntPredicate::IntSGT as u32 ),
    "<=" => BuiltinFn::Cmp( llvm::IntPredicate::IntSLE as u32 ),
    ">=" => BuiltinFn::Cmp( llvm::IntPredicate::IntSGE as u32 ),
    _ => return None
  } )
}

