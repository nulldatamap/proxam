use std::collections::hash_map::{HashMap, Entry};
use std::mem::replace;

use filemap::{CharLoc};
use ast::{Function, Type, Expression, ExpressionKind, Ident, Name, uexpr};
use builtin::{BuiltinType, builtin_type, builtin_fn};

// Code transformation and validation

// ExpressionKind::* => EK::*
mod EK {
  pub use ast::ExpressionKind::*;
}

/*

  TODO: Make it so that multiple error can be reported instead
  of one at a time.
  Fix identation
*/

#[derive(Debug)]
pub enum TransError {
  UndeclaredFunction( Function ),
  AlreadyDeclaredFunction( Function, Function ),
  AlreadyDefinedFunction( Function, Function ),
  FunctionTypeNotMatching( Function, Function ),
  InvalidArgumentList( Function ),

  UndefinedName( Name ),
  UndefinedType( Ident ),

  IncosistentIfBranches( Expression ),
  IncorrectType( Expression, Type ),
  InvalidArgumentCount( Expression ),
  UncallableValue( Expression ),

}

type TResult<T> = Result<T, TransError>;

#[derive(Debug)]
pub struct Module {
  pub name      : String,
  pub functions : HashMap<Name, Function>,
  // data structures
}

impl Module {
  fn new( name : String ) -> Module {
    Module{ name: name, functions: HashMap::new() }
  }

  fn insert_toplevel_fn( &mut self, fun : Function ) -> TResult<()> {
    self.insert_fn( fun, &mut Name::root() )
  }

  fn insert_fn( &mut self, fun : Function, scope : &mut Name ) -> TResult<()> {
    let mut fnam = scope.ident_child( &fun.name );
    fnam.no_loc(); // Remove the source location from this name
    
    // Check if the arguments of the function match it's type
    if fun.arg_names.len() != fun.ty.argument_count() {
      return Err( TransError::InvalidArgumentList( fun ) )
    }

    let entry = self.functions.entry( fnam );
    match entry {
      Entry::Vacant( va ) => { va.insert( fun ); },
      Entry::Occupied( mut oc ) => {
        match (&oc.get().body, &fun.body) {
          // In case we have to implementations
          (&Some( _ ), &Some( _ ))
            => return Err( TransError::AlreadyDefinedFunction( fun
                                                            , oc.remove() ) ),
          // In case there's already an implementation 
          // and this is just a declaration
          (&Some( _ ), &None) => {
            if oc.get().ty != fun.ty {
              return Err( TransError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
          },
          // In case there's a declaration and this is a implementation
          (&None, &Some( _ )) => {
            if oc.get().ty != fun.ty {
              return Err( TransError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
            oc.insert( fun );
          },
          // In case we have to delcarations
          (&None, &None) => {
            return Err( TransError::AlreadyDeclaredFunction( fun
                                                          , oc.remove() ) ) 
          }
        }
      }
    }

    Ok( () )
  }

  fn resolve_namespaces( &mut self ) -> TResult<()> {
    // A list of all the new bindings that might be added
    let mut binds = Vec::new();

    for (_, fun) in self.functions.iter_mut() {
      if let Some( ref mut body ) = fun.body {
        try!( Module::resolve_namespace_expr( body
                                            , &fun.arg_names[]
                                            , &mut Name::from_ident( &fun.name
                                                                   , None )
                                            , &mut binds ) );
      }
    }

    for (mut fnam, fun) in binds.into_iter() {
      try!( self.insert_fn( fun, &mut fnam ) );
    }

    Ok( () )
  }

  fn resolve_namespace_expr( expr : &mut Expression
                           , arguments : &[Ident]
                           , scope : &mut Name
                           , binds : &mut Vec<(Name, Function)> )
      -> TResult<()> {
    let ekind = &mut expr.kind;
    match ekind {
      &mut EK::UnresolvedNamed( .. ) => {
        // Take the unresolved name
        if let EK::UnresolvedNamed( idt ) = replace( ekind, EK::Invalid ) {
          // And check if it's our function's argument
          if arguments.contains( &idt ) {
            *ekind = EK::Arg( idt );
          // Or if it's one of the bound names
          } else if let Some( &(ref bn, _) ) = 
                    binds.iter()
                         .find( |&&( _, ref f )|
                            f.name.text == idt.text ) {
            let mut bnu = bn.clone();
            bnu.push( idt.text.clone() );
            *ekind = EK::Named( bnu );
          // Or a builtin function
          } else if let Some( bif ) = builtin_fn( &idt ) {
            *ekind = EK::BuiltinFn( bif );
          // Else assume it's a toplevel name
          } else {
            *ekind = EK::Named( Name::from_ident( &idt, None ) );
          }
        } 
      },
      &mut EK::Apply( ref mut exprs ) => {
        for e in exprs.iter_mut() {
          try!( Module::resolve_namespace_expr( e, arguments, scope, binds ) );
        }
      },
      &mut EK::Literal( .. ) => {},
      &mut EK::If( ref mut cnd, ref mut thn, ref mut els ) => {
        // Enter the if conditon scope
        scope.push( "if".to_string() );
        try!( Module::resolve_namespace_expr( &mut **cnd
                                            , arguments
                                            , scope
                                            , binds ) );

        // Now we're in the then scope
        scope.change( "then".to_string() );
        try!( Module::resolve_namespace_expr( &mut **thn
                                            , arguments
                                            , scope
                                            , binds ) );
   
        // And lastly the else scope
        scope.change( "else".to_string() );
        try!( Module::resolve_namespace_expr( &mut **els
                                            , arguments
                                            , scope
                                            , binds ) );

        // Reset the scope
        scope.pop();

      },
      &mut EK::Let( .. ) => {
        // Swap out the let expression from the AST
        if let EK::Let( mut fns, mut bdy ) = replace( ekind, EK::Invalid ) {
          // Resolve the namespaces for the bindings
          for fun in fns.iter_mut() {
            if let Some( ref mut bdy ) = fun.body {
              try!( Module::resolve_namespace_expr( bdy
                                                  , &fun.arg_names[]
                                                  , scope
                                                  , binds ) );
            }
          }

          // Scope the bindings and add them to the module
          for letfn in fns.into_iter() {
            binds.push( (scope.clone(), letfn) );
          }
         
          // We enter the let body scope
          scope.push( "let".to_string() );
          
          // Resolve the body
          try!( Module::resolve_namespace_expr( &mut *bdy
                                              , arguments
                                              , scope
                                              , binds ) );

          // Then replace this let node with it's body
          *ekind = bdy.kind;

          // And pop the scope
          scope.pop();
        }
      },
      inv => panic!( "Namespace resolution is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

  fn validate_names( &self ) -> TResult<()> {

    for fun in self.functions.values() {
      if let Some( ref bdy ) = fun.body {
        try!( self.validate_name_expr( bdy ) )
      }
    }

    Ok( () )
  }

  fn validate_name_expr( &self, expr : &Expression ) -> TResult<()> {
    match &expr.kind {
      &EK::Literal( .. ) 
      | &EK::Arg( .. )
      | &EK::BuiltinFn( .. ) => {}, // *BING* SKIP!
      &EK::Apply( ref exprs ) => {
        for e in exprs.iter() {
          try!( self.validate_name_expr( e ) );
        }
      },
      &EK::If( ref cnd, ref thn, ref els ) => {
        try!( self.validate_name_expr( &**cnd ) );
        try!( self.validate_name_expr( &**thn ) );
        try!( self.validate_name_expr( &**els ) );
      },
      &EK::Named( ref name ) => {
        let mut noloc_name = name.clone();
        noloc_name.no_loc();

        if !self.functions.keys().any( |k| k.same( &noloc_name ) ) {
          return Err( TransError::UndefinedName( name.clone() ) )
        }
      },
      inv => panic!( "Name validation is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

  fn resolve_applications( &mut self ) -> TResult<()> {

    for (_, fun) in self.functions.iter_mut() {
      if let Some( ref mut bdy ) = fun.body {
        try!( Module::resolve_application_expr( bdy ) );
      }
    }

    Ok( () )
  }

  fn resolve_application_expr( expr : &mut Expression ) -> TResult<()> {
    let ekind = &mut expr.kind;
    match ekind {
      &mut EK::Literal( .. )
      | &mut EK::Named( .. )
      | &mut EK::BuiltinFn( .. ) 
      | &mut EK::Arg( .. ) => {},
      &mut EK::If( ref mut cnd, ref mut thn, ref mut els ) => {
        try!( Module::resolve_application_expr( &mut **cnd ) );
        try!( Module::resolve_application_expr( &mut **thn ) );
        try!( Module::resolve_application_expr( &mut **els ) );
      },
      &mut EK::Apply( .. ) => {
        if let EK::Apply( mut elms ) = replace( ekind, EK::Invalid ) {
          if elms.len() == 0 {
            panic!( "Reached an application in the AST with zero length!" )
          }

          for arge in elms.iter_mut() {
            try!( Module::resolve_application_expr( arge ) );
          }

          let callee = elms.remove( 0 );

          // Match the to-be-applied value
          match callee.kind {
            EK::BuiltinFn( .. )
            | EK::FnCall( .. )
            | EK::If( .. )
            | EK::Literal( .. )
            | EK::Arg( .. )
            | EK::Named( .. ) =>
              *ekind = EK::FnCall( Box::new( callee ), elms ),
            ref inv => panic!( "Encountered invalid callee in application \
resolution: {:?}", inv )
          }
        }
      },
      ref inv => panic!( "Application resolution is not implemented for: {:?}"
                       , ekind )
    }

    Ok( () )
  }

  fn resolve_types( &mut self ) -> TResult<()> {

    for (n, fun) in self.functions.iter_mut() {
      try!( Module::resolve_type( &mut fun.ty ) );
      fun.ty.to_fn();
    }

    Ok( () )
  }

  fn resolve_type( ty : &mut Type ) -> TResult<()> {
    match ty {
      &mut Type::NamedType( .. ) => {
        // Unit is only a place-holder here
        if let Type::NamedType( ty_name ) = replace( ty, Type::Unit ) {
          if let Some( bit ) = builtin_type( &ty_name ) {
            *ty = Type::BuiltinType( bit );
          } else {
            return Err( TransError::UndefinedType( ty_name ) )
          }
        }
      },
      &mut Type::Unit
      | &mut Type::BuiltinType( .. ) => {},
      &mut Type::Tuple( ref mut elms ) => {
        for elm in elms.iter_mut() {
          try!( Module::resolve_type( elm ) );
        }
      },
      &mut Type::List( ref mut inner ) => {
        try!( Module::resolve_type( &mut **inner ) );
      },
      &mut Type::Fn( ref mut args, ref mut ret ) => {
        for arg in args.iter_mut() {
          try!( Module::resolve_type( arg ) );
        }

        try!( Module::resolve_type( &mut **ret ) );
      },
      inv => panic!( "Type resolution not implemented for: {:?}", inv )
    }
    Ok( () )
  }

  /*
    TODO:
    "Abstract" function types
    Type functions ( generics )
    Partial application
    Envoriment capturing
  */

  fn check_types( &mut self ) -> TResult<()> {
    // Walk through the tree nodes, check for expectations and incosistentcies
    // Check function call arguments; partial application, no arguments
    // and too many arguments ( and application overflow into the returned value )
    
    for (_, fun) in self.functions.iter_mut() {
      if let Some( ref mut bdy ) = fun.body {
        try!( Module::type_check_expr( bdy ) );
        // Coerce the return expression value if needed, or fail the type check
        let rty = if fun.ty.is_fn_type() {
          fun.ty.return_type()
        } else {
          &fun.ty
        };
        try!( Module::coerce_expr( bdy, rty ) );
      }
    }

    Ok( () )
  }

  fn type_check_expr( expr : &mut Expression ) -> TResult<()> {
    // Get the type and kind seperately
    let &mut Expression{ kind: ref mut ekind, ty: ref mut ety } = expr;
    
    match ekind {
      &mut EK::If( .. ) => {
        if let &mut EK::If( ref mut cnd, ref mut thn, ref mut els ) = ekind {
          try!( Module::type_check_expr( &mut **cnd ) );
          try!( Module::type_check_expr( &mut **thn ) );
          try!( Module::type_check_expr( &mut **els ) );

          // The condition must always be boolean
          let boolty = Type::BuiltinType( BuiltinType::Bool );
          try!( Module::coerce_expr( &mut **cnd
                                         , &boolty ) );
          // If the two branches don't match, try to coerce them before 
          // we report an error
          if thn.ty != els.ty {
            // Because the initial type of an if-expressino is set to the type
            // of the `then` expression, we have to remember to update it to
            // match the new type of the `then` expression ( which now matches
            // `else` )
            if Module::coerce_expr( thn, &els.ty ).is_ok() {
              *ety = thn.ty.clone();
              return Ok( () )
            } else if Module::coerce_expr( els, &thn.ty ).is_ok() {
              // Nothing to do because the if expression type is already correct
              return Ok( () )
            }
          }
        }
        let errexpr = Expression::new( replace( ekind, EK::Invalid )
                                     , ety.clone() );
        return Err( TransError::IncosistentIfBranches( errexpr ) )
      },
      &mut EK::Literal( .. )
      | &mut EK::Arg( .. )
      | &mut EK::Named( .. )
      | &mut EK::BuiltinFn( .. ) => {}, // Skip
      &mut EK::FnCall( ref mut callee, ref mut args ) => {
        // Type checl the args and callee
        try!( Module::type_check_expr( callee ) );
        for arg in args.iter_mut() {
          try!( Module::type_check_expr( arg ) );
        }

        if !callee.ty.is_fn_type() {
          // Just a bug test
          if args.len() == 0 {
            panic!( "Found a non-function call with zero arguments! {:?}"
                  , callee )
          }
          // Report an error, we can't call a non-function value
          let errexpr = Expression::new( EK::FnCall( callee.clone()
                                                   , args.clone() )
                                       , ety.clone() );
          return Err( TransError::UncallableValue( errexpr ) )
        }
        // Check for partial application
        if callee.ty.argument_count() > args.len() {
          panic!( "Partial application is not implemented: {:?}", callee )
        }
        // Check for over-application
        if callee.ty.argument_count() < args.len() {
          // If the current function returns a function, we assume it's
          // over application
          if callee.ty.return_type().is_fn_type() {
            panic!( "Over application is not implemented: {:?}", callee )
          // Otherwise we assue that there's just too many arguments
          } else {
            let errexpr = Expression::new( EK::FnCall( callee.clone()
                                                     , args.clone() )
                                         , ety.clone() );
            return Err( TransError::InvalidArgumentCount( errexpr ) )
          }
        }

        for (i, arg) in args.iter_mut().enumerate() {
          try!( Module::coerce_expr( arg, callee.ty.argument_type( i ) ) );
        }

      },
      inv => panic!( "Type checking is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

  fn coerce_expr( expr : &mut Expression, ty : &Type ) -> TResult<()> {
    // There can be multiple layers of coercion, so we'll keep looping
    loop {
      // Just return early then
      if &expr.ty == ty {
        return Ok( () )
      }

      // If we can get the right type by calling the function
      if expr.ty.is_call_coercible( ty ) {
        let innerkind = replace( &mut expr.kind, EK::Invalid );
        // Turn it into the right function call
        expr.kind = {
          let innerexpr = Expression::new( innerkind, expr.ty.clone() );
          // Or just a normal function call otherwise
          EK::FnCall( Box::new( innerexpr ), Vec::new() )
        };
        let t = replace( &mut expr.ty, Type::Untyped );
        if let Type::Fn( _, ret ) = t {
          expr.ty = *ret;
        } else {
          panic!( "Got unexpected type while coercing: {:?}", t )
        }
      // We're out of options, the types just don't match
      } else {
        // So let's just report the error
        // We take the expression out of tree since it's not needed in it, and
        // we need the expression by value
        let errexpr = replace( expr, uexpr( EK::Invalid ) );

        return Err( TransError::IncorrectType( errexpr, ty.clone() ) )
      }
    }

    Ok( () )
  }

  fn type_annotate( &mut self ) -> TResult<()> {

    let mut typemap : HashMap<Name, Type> =
      self.functions.iter()
                    .map( |(k, v)| (k.clone(), v.ty.clone()) )
                    .collect();

    for (_, fun) in self.functions.iter_mut() {
      if let Some( ref mut bdy ) = fun.body {
        try!( Module::type_annotate_expr( bdy, &fun.arg_names[]
                                        , &fun.ty, &mut typemap ) );
      }
    }

    Ok( () )
  }

  // Walks through the tree and annotate the expr.ty for the
  // type checker to validate later down the pipeline.
  fn type_annotate_expr( expr : &mut Expression
                       , args : &[Ident]
                       , fty  : &Type
                       , typemap : &mut HashMap<Name, Type> ) -> TResult<()> {
    let &mut Expression{ kind: ref mut ekind, ty: ref mut ety  } = expr;

    match ekind {
      &mut EK::If( ref mut cnd, ref mut thn, ref mut els ) => {
        // Annotate all the sub-expressions
        try!( Module::type_annotate_expr( &mut **cnd, args, fty, typemap ) );
        try!( Module::type_annotate_expr( &mut **thn, args, fty, typemap ) );
        try!( Module::type_annotate_expr( &mut **els, args, fty, typemap ) );

        // A if-expresionn takes the type of it's branches
        // Branch type consistency will be checked later
        // so for now we'll just assume that they both
        // have the same type.
        *ety = thn.ty.clone();
      },
      &mut EK::Literal( .. ) => {},
      &mut EK::Arg( ref idt ) => {
        let idx = args.iter().position( |v| v.text == idt.text )
                             .expect( "Encountered non-existant arg." );
        *ety = fty.argument_type( idx ).clone();
      },
      &mut EK::Named( ref mut fnam_spot ) => {
        // Swap out the Name so we can own it for a little
        let mut fnam = replace( fnam_spot, Name::root() );
        // Swap out the location temporarely so we can get a source-position
        // independant hash of the name.
        let fnloc = replace( &mut fnam.loc, None );
        *ety = typemap.get( &fnam ).expect( "Encountered non-existant fn." )
                                  .clone();
        // Then restore it for possible later use
        fnam.loc = fnloc;
        // And swap the name back into the tree
        let _ = replace( fnam_spot, fnam );
      },
      &mut EK::BuiltinFn( ref bif ) => {
        *ety = bif.get_type();
      },
      &mut EK::FnCall( ref mut f, ref mut fargs ) => {
        for arg in fargs.iter_mut() {
          try!( Module::type_annotate_expr( arg, args, fty, typemap ) );
        }

        try!( Module::type_annotate_expr( &mut **f, args, fty, typemap ) );

        *ety = f.ty.return_type()
                   .clone();
      }
      inv => panic!( "Type annotation is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

}

pub fn validate_module( name : String, fns : Vec<Function> )
       -> TResult<Module> {
  let mut module = Module::new( name );
  
  for fun in fns.into_iter() {
    try!( module.insert_toplevel_fn( fun ) );
  }

  try!( module.resolve_namespaces() );
  try!( module.validate_names() );
  try!( module.resolve_applications() );
  try!( module.resolve_types() );
  try!( module.type_annotate() );
  try!( module.check_types() );
  
  Ok( module )
}


