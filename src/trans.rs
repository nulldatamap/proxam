use std::collections::hash_map::{HashMap, Entry};
use std::mem::replace;

use filemap::{CharLoc};
use ast::{Function, Type, Expression, Ident, Name};
use builtin::{builtin_type, builtin_fn};

// Code transformation and validation

/*

  TODO: Make it so that multiple error can be reported instead
  of one at a time.

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
    if fun.arg_names.len() as u32 != fun.ty.argument_count() {
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
    match expr {
      &mut Expression::UnresolvedNamed( .. ) => {
        // Take the unresolved name
        if let Expression::UnresolvedNamed( idt ) =
               replace( expr, Expression::Invalid ) {
          // And check if it's our function's argument
          if arguments.contains( &idt ) {
            *expr = Expression::Arg( idt );
          // Or if it's one of the bound names
          } else if binds.iter()
                         .any( |&( ref n, _ )|
                            n.matches( scope, &idt.text[] ) ) {
            *expr = Expression::Named( scope.ident_child( &idt ) );
          // Or a builtin function
          } else if let Some( bif ) = builtin_fn( &idt ) {
            *expr = Expression::BuiltinFn( bif );
          // Else assume it's a toplevel name
          } else {
            *expr = Expression::Named( Name::from_ident( &idt, None ) );
          }
        } 
      },
      &mut Expression::Apply( ref mut exprs ) => {
        for e in exprs.iter_mut() {
          try!( Module::resolve_namespace_expr( e, arguments, scope, binds ) );
        }
      },
      &mut Expression::Literal( .. ) => {},
      &mut Expression::If( ref mut cnd, ref mut thn, ref mut els ) => {
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
      &mut Expression::Let( .. ) => {
        // Swap out the let expression from the AST
        if let Expression::Let( mut fns, bdy )
             = replace( expr, Expression::Invalid ) {
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
         
          // Then replace this let node with it's body
          *expr = *bdy;
         
          // We enter the let body scope
          scope.push( "let".to_string() );
          
          // And resolve the body
          try!( Module::resolve_namespace_expr( expr
                                              , arguments
                                              , scope
                                              , binds ) );

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
    match expr {
      &Expression::Literal( .. ) 
      | &Expression::Arg( .. )
      | &Expression::BuiltinFn( .. ) => {}, // *BING* SKIP!
      &Expression::Apply( ref exprs ) => {
        for e in exprs.iter() {
          try!( self.validate_name_expr( e ) );
        }
      },
      &Expression::If( ref cnd, ref thn, ref els ) => {
        try!( self.validate_name_expr( &**cnd ) );
        try!( self.validate_name_expr( &**thn ) );
        try!( self.validate_name_expr( &**els ) );
      },
      &Expression::Named( ref name ) => {
        if name.is_toplevel() {

          if !self.functions.keys().any( |k| k.same( name ) ) {
            return Err( TransError::UndefinedName( name.clone() ) )
          }
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
    match expr {
      &mut Expression::Literal( .. )
      | &mut Expression::Named( .. )
      | &mut Expression::BuiltinFn( .. ) 
      | &mut Expression::Arg( .. ) => {},
      &mut Expression::If( ref mut cnd, ref mut thn, ref mut els ) => {
        try!( Module::resolve_application_expr( &mut **cnd ) );
        try!( Module::resolve_application_expr( &mut **thn ) );
        try!( Module::resolve_application_expr( &mut **els ) );
      },
      &mut Expression::Apply( .. ) => {
        if let Expression::Apply( mut elms ) = replace( expr
                                                      , Expression::Invalid ) {
          if elms.len() == 0 {
            panic!( "Reached an application in the AST with zero length!" )
          }
          let callee = elms.remove( 0 );

          for arge in elms.iter_mut() {
            try!( Module::resolve_application_expr( arge ) );
          }

          // Match the to-be-applied value
          match callee {
            Expression::BuiltinFn( bif ) =>
              *expr = Expression::BuiltinCall( bif, elms ),
            Expression::If( .. )
            | Expression::Literal( .. )
            | Expression::Arg( .. )
            | Expression::Named( .. ) =>
              *expr = Expression::FnCall( Box::new( callee ), elms ),
            ref inv => panic!( "Encountered invalid callee in application \
resolution: {:?}", inv )
          }
        }
      },
      ref inv => panic!( "Application resolution is not implemented for: {:?}"
                   , expr )
    }

    Ok( () )
  }

  fn resolve_types( &mut self ) -> TResult<()> {

    for (_, fun) in self.functions.iter_mut() {
      try!( Module::resolve_type( &mut fun.ty ) );
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
      &mut Type::Unit => {},
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

  fn check_types( &mut self ) -> TResult<()> {
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
  try!( module.check_types() );
  
  Ok( module )
}


