use std::ffi::CString;
use std::collections::HashMap;

use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef
                 , TypeRef, BuilderRef, BasicBlockRef};

use tokenizer;
use builtin::{BuiltinType, BuiltinFn};
use ast::{Function, Type, Expression, ExpressionKind, Literal, Ident};
use trans::{Module};

type ValueMap = HashMap<String, ValueRef>;

// ExpressionKind::* => EK::*
mod EK {
  pub use ast::ExpressionKind::*;
}

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
      let ftype = self.get_bare_type( &func.ty.to_fn() );
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
        let entry = llvm::LLVMAppendBasicBlockInContext( self.context
                                                       , f
                                                       , noname() );
        
        let builder = llvm::LLVMCreateBuilderInContext( self.context );
        
        llvm::LLVMPositionBuilderAtEnd( builder, entry );

        llvm::LLVMBuildRet( builder
                          , self.get_expression_value( builder
                                                     , f, &func.arg_names[]
                                                     , body ) );
      }

    }
  }

  fn get_bare_fn_type( &mut self, args : &[Type], ret : &Type ) -> TypeRef {
    let targs : Vec<TypeRef> = args.iter()
                                   .map( |t| self.get_type( t ) )
                                   .collect();
    unsafe {
      llvm::LLVMFunctionType( self.get_type( ret )
                            , targs.as_ptr(), targs.len() as u32, 0 )
    }
  }

  fn get_fn_type( &mut self, args : &[Type], ret : &Type ) -> TypeRef {
    unsafe {
      llvm::LLVMPointerType( self.get_bare_fn_type( args, ret ), 0 )
    }
  }

  fn get_type( &mut self, ty : &Type ) -> TypeRef {
    match ty {
      &Type::BuiltinType( ref bit ) => bit.as_llvm_type( self.context ),
      &Type::Tuple( ref el ) => self.get_tuple_type( &el[] ),
      &Type::Unit => self.unit_type(),
      &Type::Fn( ref args, ref ret ) => self.get_fn_type( &args[], &**ret ),
      n => panic!( "Reached unresolved type in codegen: {:?}", n )
    }
  }

  fn unit_type( &mut self ) -> TypeRef {
    unsafe {
      llvm::LLVMStructTypeInContext( self.context, 0 as *const TypeRef, 0, 0 )
    }
  }

  fn get_bare_type( &mut self, ty : &Type ) -> TypeRef {
    match ty {
      &Type::Fn( ref args, ref ret ) => self.get_bare_fn_type( &args[], &**ret ),
      v => self.get_type( v )
    }
  }

  fn get_tuple_type( &mut self, tys : &[Type] ) -> TypeRef {
    let lltys : Vec<TypeRef> = tys.iter()
                                  .map( |v| self.get_type( v ) )
                                  .collect();
    unsafe {
      llvm::LLVMStructTypeInContext( self.context, lltys.as_ptr()
                                   , lltys.len() as u32, 0 )
    }
  }

  fn get_expression_value( &mut self, builder : BuilderRef
                         , fun : ValueRef, args : &[Ident]
                         , expr : Expression ) -> ValueRef {
    match expr.kind {
      EK::Literal( lit ) => self.get_literal_value( builder, lit ),
      EK::BuiltinCall( bif, bargs ) => {
        let vargs : Vec<ValueRef>;
        vargs = bargs.into_iter()
                     .map( |v| self.get_expression_value( builder, fun
                                                        , args, v ) )
                     .collect();

        bif.as_llvm_value( builder, &vargs[] )
      },
      EK::Arg( name ) => self.get_argument_value( fun, args, name ),
      EK::If( cnd, thn, els ) => self.get_if_value( builder, fun, args
                                                          , *cnd, *thn, *els ),
      inv => panic!( "Codegen not implemented for expression: {:?}", inv )
    }
  }

  fn get_argument_value( &mut self, fun : ValueRef, args : &[Ident]
                       , name : Ident ) -> ValueRef {
    let i = args.position_elem( &name ).expect( "Invalid argument in codegen." );
    unsafe {
      llvm::get_param( fun, i as u32 )
    }
  }

  fn get_if_value( &mut self, builder : BuilderRef, fun : ValueRef
                 , args : &[Ident] , cnd : Expression, thn : Expression
                 , els : Expression ) -> ValueRef {
    unsafe {
      
      let ifty = self.get_type( &thn.ty );

      let cndv = self.get_expression_value( builder, fun, args, cnd );
      let thnv = self.get_expression_value( builder, fun, args, thn );
      let elsv = self.get_expression_value( builder, fun, args, els );
      let tbb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, noname() );
      let ebb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, noname() );
      let rbb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, noname() );
      llvm::LLVMBuildCondBr( builder, cndv, tbb, ebb );
      let rphi = llvm::LLVMBuildPhi( builder, ifty
                                   , noname() );

      let bbs = [ tbb, ebb ];
      let vs = [ thnv, elsv ];

      llvm::LLVMAddIncoming( rphi, &vs as *const ValueRef
                           , &bbs as *const BasicBlockRef, 2 );

      llvm::LLVMPositionBuilderAtEnd( builder, tbb );
      llvm::LLVMBuildBr( builder, rbb );
      llvm::LLVMPositionBuilderAtEnd( builder, tbb );
      llvm::LLVMBuildBr( builder, rbb );

      rphi
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



