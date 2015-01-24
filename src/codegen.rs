use std::ffi::CString;

use rustc::llvm;

use ast::{Function, Type, BuiltinType};
use hicr::{Module};

type ContextRef = *mut llvm::Context_opaque;
type ValueRef = *mut llvm::Value_opaque;
type ModuleRef = *mut llvm::Module_opaque;
type TypeRef = *mut llvm::Type_opaque;

fn unit_type( ctx : ContextRef ) -> TypeRef {
  unsafe {
    llvm::LLVMStructTypeInContext( ctx, 0 as *const TypeRef, 0, 0 )
  }
}

pub fn generate_module( name : &str, module : Module ) -> ModuleRef {
  let llmodule;

  unsafe {
    let ctx = llvm::LLVMContextCreate();

    let c_name = CString::from_slice( name.as_bytes() );
    llmodule = llvm::LLVMModuleCreateWithNameInContext( c_name.as_ptr(), ctx );
    
    for (fname, func) in module.functions.into_iter() {
      generate_fn( ctx, &fname[], func, llmodule );
    }

  }

  llmodule
}

fn generate_fn( ctx : ContextRef, fname : &str
              , func : Function, module : ModuleRef ) {
  unsafe {
    let ftype = get_bare_type( ctx, func.ty.to_fn() );
    let cname = CString::from_slice( fname.as_bytes() );
    let f = llvm::LLVMGetOrInsertFunction( module, cname.as_ptr(), ftype );
    
    for (arg, i) in func.arg_names.iter().zip( range( 0, func.arg_names.len() ) ) {
      let carg = CString::from_slice( arg.text.as_bytes() );
      llvm::LLVMSetValueName( llvm::LLVMGetParam( f, i as u32 ), carg.as_ptr() );
    }

    let centry = CString::from_slice( "entry".as_bytes() );
    
    let entry = llvm::LLVMAppendBasicBlockInContext( ctx, f, centry.as_ptr() );
    
    let builder = llvm::LLVMCreateBuilderInContext( ctx );
    
    llvm::LLVMPositionBuilderAtEnd( builder, entry );
    llvm::LLVMBuildRetVoid( builder );
  }
}

fn get_bare_fn_type( ctx : ContextRef, args : Vec<Type>, ret : Type ) -> TypeRef {
  let targs : Vec<TypeRef> = args.into_iter()
                                 .map( |t| get_type( ctx, t ) )
                                 .collect();
  unsafe {
    llvm::LLVMFunctionType( get_type( ctx, ret )
                          , targs.as_ptr(), targs.len() as u32, 0 )
  }
}

fn get_fn_type( ctx : ContextRef, args : Vec<Type>, ret : Type ) -> TypeRef {
  unsafe {
    llvm::LLVMPointerType( get_bare_fn_type( ctx, args, ret ), 0 )
  }
}

fn get_type( ctx : ContextRef, ty : Type ) -> TypeRef {
  match ty {
    Type::BuiltinType( bit ) => get_builtin_type( ctx, bit ),
    Type::Tuple( el ) => get_tuple_type( ctx, el ),
    Type::Unit => unit_type( ctx ),
    Type::Fn( args, ret ) => get_fn_type( ctx, args, *ret ),
    /*Type::NamedType( n )*/n => panic!( "Reached unresolved type in codegen: {:?}"
                                  , n )
  }
}

fn get_bare_type( ctx : ContextRef, ty : Type ) -> TypeRef {
  match ty {
    Type::Fn( args, ret ) => get_bare_fn_type( ctx, args, *ret ),
    v => get_type( ctx, v )
  }
}

fn get_builtin_type( ctx : ContextRef, bit : BuiltinType ) -> TypeRef {
  unsafe {
    match bit {
      BuiltinType::Int => llvm::LLVMInt32TypeInContext( ctx )
    }
  }
}

fn get_tuple_type( ctx : ContextRef, tys : Vec<Type> ) -> TypeRef {
  let lltys : Vec<TypeRef> = tys.into_iter()
                                .map( |v| get_type( ctx, v ) )
                                .collect();
  unsafe {
    llvm::LLVMStructTypeInContext( ctx, lltys.as_ptr()
                                 , lltys.len() as u32, 0 )
  }
}

