use std::ffi::CString;
use std::collections::HashMap;
use std::mem::transmute;

use rustc::llvm;
use rustc::llvm::{ContextRef, ValueRef, ModuleRef
                 , TypeRef, BuilderRef, BasicBlockRef};

use tokenizer;
use builtin::{BuiltinType, BuiltinFn};
use ast::{Function, Type, Expression, ExpressionKind, Literal, Ident, Name};
use trans::{Module};

type ValueMap = HashMap<String, ValueRef>;

// ExpressionKind::* => EK::*
mod EK {
  pub use ast::ExpressionKind::*;
}

pub struct Codegen {
  functions : ValueMap,
  module    : ModuleRef,
  context   : ContextRef,
  builder   : BuilderRef
}

// A zero length c-string, (just like in rustc)
pub fn noname() -> *const i8 {
  static C_STR_TERM : i8 = 0;
  &C_STR_TERM
}

pub fn cstr( s : &'static str ) -> *const i8 {
  s.as_ptr() as *const i8
}

impl Codegen {

  fn new( lm : ModuleRef, c : ContextRef, b : BuilderRef ) -> Codegen {
    Codegen { functions: HashMap::new()
            , module: lm
            , context: c
            , builder: b }
  }

  pub fn generate_module( module : Module ) -> ModuleRef {
    let mut codegen;

    unsafe {
      let ctx = llvm::LLVMContextCreate();

      let c_name = CString::new( module.name.as_bytes() ).unwrap();
      let llmodule = llvm::LLVMModuleCreateWithNameInContext( c_name.as_ptr()
                                                            , ctx );
      
      let builder = llvm::LLVMCreateBuilderInContext( ctx );

      codegen = Codegen::new( llmodule, ctx, builder );

      codegen.generate_fns( module );

    }

    codegen.module
  }

  fn generate_fns( &mut self, module : Module ) {
    let mut building_parts = Vec::new();
    // Register all the functions so referencing won't be a problem
    for (fname, func) in module.functions.into_iter() {
      if let Some( r ) = self.register_fn( fname.to_string(), func ) {
        building_parts.push( r );
      }
    }

    for (llfn, fnargs, fnexpr) in building_parts.into_iter() {
      self.build_fn( llfn, fnargs, fnexpr );
    }

  }
  
  fn register_fn( &mut self, fname : String, func : Function )
     -> Option<(ValueRef, Vec<Ident>, Expression)> {

    let Function{ name: _, ty: fnty, arg_names: fnargs
                , body: fnbody, constraints: _ } = func;

    unsafe {
      let ftype = self.get_bare_type( &fnty );
      let cname = CString::new( fname.as_bytes() ).unwrap();
      let f = llvm::LLVMGetOrInsertFunction( self.module
                                           , cname.as_ptr()
                                           , ftype );

      // Insert the ValueRef of the function into the functions
      self.functions.insert( fname, f );

      for (arg, i) in fnargs.iter().zip( range( 0, fnargs.len() ) ) {
        let carg = CString::new( arg.text.as_bytes() ).unwrap();
        let prm = llvm::get_param( f, i as u32 );

        llvm::LLVMSetValueName( prm, carg.as_ptr() );
      }

      if let Some( b ) = fnbody {
        Some( (f, fnargs, b) )
      } else {
        None
      }
    }
  }

  fn build_fn( &mut self, llfn : ValueRef, args : Vec<Ident>
             , body : Expression ) {
    unsafe {
      let entry = llvm::LLVMAppendBasicBlockInContext( self.context
                                                     , llfn
                                                     , noname() );
        
      llvm::LLVMPositionBuilderAtEnd( self.builder, entry );

      llvm::LLVMBuildRet( self.builder
                        , self.get_expression_value( llfn, &args
                                                   , body ) );
    }
  }

  //
  // Types
  //

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
      &Type::Tuple( ref el ) => self.get_tuple_type( &el ),
      &Type::Unit => self.get_unit_type(),
      &Type::Fn( ref args, ref ret ) => self.get_fn_type( &args, &**ret ),
      n => panic!( "Reached unresolved type in codegen: {:?}", n )
    }
  }

  fn get_unit_type( &mut self ) -> TypeRef {
    unsafe {
      llvm::LLVMStructTypeInContext( self.context, 0 as *const TypeRef, 0, 0 )
    }
  }

  fn get_bare_type( &mut self, ty : &Type ) -> TypeRef {
    match ty {
      &Type::Fn( ref args, ref ret ) => self.get_bare_fn_type( &args, &**ret ),
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

  //
  // Expressions
  //

  fn get_expression_value( &mut self, fun : ValueRef, args : &[Ident]
                         , expr : Expression ) -> ValueRef {
    match expr.kind {
      EK::Literal( lit ) => self.get_literal_value( lit ),
      EK::BuiltinFn( bif ) => {
        panic!( "Reached a BuiltinFn in codegen outside a call: {:?}", bif );
      },
      EK::Arg( name ) => self.get_argument_value( fun, args, name ),
      EK::If( cnd, thn, els ) => self.get_if_value( fun, args, *cnd, *thn, *els ),
      EK::FnCall( f, fargs ) => self.get_fn_call_value( fun, args, *f, fargs ),
      EK::Named( name ) => self.get_named_value( name ),
      inv => panic!( "Codegen not implemented for expression: {:?}", inv )
    }
  }

  fn get_argument_value( &mut self, fun : ValueRef, args : &[Ident]
                       , name : Ident ) -> ValueRef {
    let i = args.position_elem( &name ).expect( "Invalid argument in codegen." );
    llvm::get_param( fun, i as u32 )
  }

  fn get_if_value( &mut self, fun : ValueRef, args : &[Ident]
                 , cnd : Expression, thn : Expression, els : Expression )
     -> ValueRef {
    unsafe {
      let ifty = self.get_type( &thn.ty );
      // Condition
      let cndv = self.get_expression_value( fun, args, cnd );
      let tbb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, cstr( "_then\0" ) );
      let ebb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, cstr( "_else\0" ) );
      let mbb = llvm::LLVMAppendBasicBlockInContext( self.context
                                                   , fun, cstr( "_merge\0" ) );
      llvm::LLVMBuildCondBr( self.builder, cndv, tbb, ebb );
      // Then
      llvm::LLVMPositionBuilderAtEnd( self.builder, tbb );
      let thnv = self.get_expression_value( fun, args, thn );
      llvm::LLVMBuildBr( self.builder, mbb );
      // Else
      llvm::LLVMPositionBuilderAtEnd( self.builder, ebb );
      let elsv = self.get_expression_value( fun, args, els );
      llvm::LLVMBuildBr( self.builder, mbb );
      // Merge
      llvm::LLVMPositionBuilderAtEnd( self.builder, mbb );
      let mphi = llvm::LLVMBuildPhi( self.builder, ifty
                                   , noname() );
      // We need to find the incoming block, now if it's a constant it can only
      // be coming from the then/else basic block, else we get the parent block
      // of the incoming value.
      let bbs =
      [ if llvm::LLVMIsConstant( thnv ) == 0 {
          llvm::LLVMGetInstructionParent( thnv )
        } else {
          tbb

        }, if llvm::LLVMIsConstant( elsv ) == 0 {
          llvm::LLVMGetInstructionParent( elsv )
        } else {
          ebb
        } ];
      let vs = [ thnv, elsv ];

      llvm::LLVMAddIncoming( mphi, &vs as *const ValueRef
                           , &bbs as *const BasicBlockRef, 2 );
      

      mphi
    }
  }

  fn get_fn_call_value( &mut self, fun : ValueRef, args : &[Ident]
                      , calledfn : Expression, fargs : Vec<Expression> )
     -> ValueRef {
    let fargsvs : Vec<ValueRef> =
      fargs.into_iter()
           .map( |e| self.get_expression_value( fun, args, e ) )
           .collect();
    unsafe {
      if let EK::BuiltinFn( bif ) = calledfn.kind {
        return bif.as_llvm_value( self.builder, &fargsvs )
      }
      llvm::LLVMBuildCall( self.builder, self.get_expression_value( fun, args
                                                                  , calledfn )
                        , fargsvs.as_ptr(), fargsvs.len() as u32, noname() )
    }
  }

  fn get_named_value( &mut self, mut name : Name ) -> ValueRef {
    *self.functions.get( &name.to_string() )
                   .expect( "Expted function in codegen." )
  }

  fn get_literal_value( &mut self, lit : Literal )
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
        tokenizer::Literal::Unit
          => llvm::LLVMConstStructInContext( self.context, 0 as *const ValueRef
                                           , 0, 0 )
      }
    }
  }
}



