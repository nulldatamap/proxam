use std::ffi::CString;
use std::collections::HashMap;

use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef, TypeRef, BuilderRef};

use tokenizer;
use builtin::{BuiltinType, BuiltinFn};
use ast::{Function, Type, Expression, Literal};
use trans::{Module};

type ValueMap = HashMap<String, ValueRef>;

pub struct Codegen {
  functions : ValueMap,
  module  : ModuleRef,
  context   : ContextRef
}

// A zero length c-string, (just like in rustc)
pub fn noname() -> *const i8 {
  static C_STR_TERM : i8 = 0;
  &C_STR_TERM
}

impl Codegen {

  fn new( lm : ModuleRef, c : ContextRef ) -> Codegen {
    Codegen { functions: HashMap::new()
            , module: lm
            , context: c }
  }

  pub fn generate_module( module : Module ) -> ModuleRef {
    let mut codegen;

    unsafe {
      let ctx = llvm::LLVMContextCreate();

      let c_name = CString::from_slice( module.name.as_bytes() );
      let llmodule = llvm::LLVMModuleCreateWithNameInContext( c_name.as_ptr()
                                                            , ctx );
      
      codegen = Codegen::new( llmodule, ctx );

      codegen.generate_fns( module );

    }

    codegen.module
  }

  fn generate_fns( &mut self, module : Module ) {
    for (fname, func) in module.functions.into_iter() {
      self.generate_fn( fname.to_string(), func );
    }
  }
  
  fn generate_fn( &mut self, fname : String, func : Function ) {
    unsafe {
      let ftype = self.get_bare_type( func.ty.to_fn() );
      let cname = CString::from_slice( fname.as_bytes() );
      let f = llvm::LLVMGetOrInsertFunction( self.module
                                           , cname.as_ptr()
                                           , ftype );

      // Insert the ValueRef of the function into the functions
      self.functions.insert( fname, f );

      for (arg, i) in func.arg_names.iter().zip( range( 0, func.arg_names.len() ) ) {
        let carg = CString::from_slice( arg.text.as_bytes() );
        let prm = llvm::get_param( f, i as u32 );

        llvm::LLVMSetValueName( prm, carg.as_ptr() );
      }

      if let Some( body ) = func.body {
        let centry = CString::from_slice( "entry".as_bytes() );
        
        let entry = llvm::LLVMAppendBasicBlockInContext( self.context
                                                       , f
                                                       , centry.as_ptr() );
        
        let builder = llvm::LLVMCreateBuilderInContext( self.context );
        
        llvm::LLVMPositionBuilderAtEnd( builder, entry );
        println!("Building function body");
        llvm::LLVMBuildRet( builder
                          , self.get_expression_value( builder, f, body ) );
      }

    }
  }

  fn get_bare_fn_type( &mut self, args : Vec<Type>, ret : Type ) -> TypeRef {
    let targs : Vec<TypeRef> = args.into_iter()
                                   .map( |t| self.get_type( t ) )
                                   .collect();
    unsafe {
      llvm::LLVMFunctionType( self.get_type( ret )
                            , targs.as_ptr(), targs.len() as u32, 0 )
    }
  }

  fn get_fn_type( &mut self, args : Vec<Type>, ret : Type ) -> TypeRef {
    unsafe {
      llvm::LLVMPointerType( self.get_bare_fn_type( args, ret ), 0 )
    }
  }

  fn get_type( &mut self, ty : Type ) -> TypeRef {
    match ty {
      Type::BuiltinType( bit ) => bit.as_llvm_type( self.context ),
      Type::Tuple( el ) => self.get_tuple_type( el ),
      Type::Unit => self.unit_type(),
      Type::Fn( args, ret ) => self.get_fn_type( args, *ret ),
      n => panic!( "Reached unresolved type in codegen: {:?}", n )
    }
  }

  fn unit_type( &mut self ) -> TypeRef {
    unsafe {
      llvm::LLVMStructTypeInContext( self.context, 0 as *const TypeRef, 0, 0 )
    }
  }

  fn get_bare_type( &mut self, ty : Type ) -> TypeRef {
    match ty {
      Type::Fn( args, ret ) => self.get_bare_fn_type( args, *ret ),
      v => self.get_type( v )
    }
  }

  fn get_tuple_type( &mut self, tys : Vec<Type> ) -> TypeRef {
    let lltys : Vec<TypeRef> = tys.into_iter()
                                  .map( |v| self.get_type( v ) )
                                  .collect();
    unsafe {
      llvm::LLVMStructTypeInContext( self.context, lltys.as_ptr()
                                   , lltys.len() as u32, 0 )
    }
  }

  fn get_expression_value( &mut self, builder : BuilderRef
                         , fun : ValueRef, expr : Expression ) -> ValueRef {
    match expr {
      Expression::Literal( lit ) => self.get_literal_value( builder, lit ),
      Expression::BuiltinCall( bif, args ) => {
        let vargs : Vec<ValueRef>;
        vargs = args.into_iter()
                    .map( |v| self.get_expression_value( builder, fun, v ) )
                    .collect();

        bif.as_llvm_value( builder, &vargs[] )
      },
      //Expression::Arg( name ) => get_argument_value( func, name ),
      inv => panic!( "Codegen not implemented for expression: {:?}", inv )
    }
  }

  fn get_literal_value( &mut self, builder : BuilderRef, lit : Literal )
     -> ValueRef {
    unsafe {
      match lit.lit {
        tokenizer::Literal::Integer( iv )
          => llvm::LLVMConstInt( BuiltinType::Int.as_llvm_type( self.context )
                               , iv as u64
                               , 0 ),
        tokenizer::Literal::Boolean( bl )
          => llvm::LLVMConstInt( BuiltinType::Bool.as_llvm_type( self.context )
                               , if bl { 1 } else { 0 }
                              , 0 ),
      }
    }
  }
}



