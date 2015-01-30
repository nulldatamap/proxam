use std::collections::hash_map::{HashMap, Entry};
use std::mem::replace;

use filemap::{CharLoc};
use ast::{Function, Type, Expression, Ident, Name};

// Highlevel Intermediate Code Representation

#[derive(Debug)]
pub enum HicrError {
  UndeclaredFunction( Function ),
  AlreadyDeclaredFunction( Function, Function ),
  AlreadyDefinedFunction( Function, Function ),
  FunctionTypeNotMatching( Function, Function ),

  UndefinedName( Name ),
}

type HResult<T> = Result<T, HicrError>;

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

  fn insert_toplevel_fn( &mut self, fun : Function ) -> HResult<()> {
    self.insert_fn( fun, &mut Name::root() )
  }

  fn insert_fn( &mut self, fun : Function, scope : &mut Name ) -> HResult<()> {
    let mut fnam = scope.ident_child( &fun.name );
    fnam.no_loc(); // Remove the source location from this name
    
    let entry = self.functions.entry( fnam );
    match entry {
      Entry::Vacant( va ) => { va.insert( fun ); },
      Entry::Occupied( mut oc ) => {
        match (&oc.get().body, &fun.body) {
          // In case we have to implementations
          (&Some( _ ), &Some( _ ))
            => return Err( HicrError::AlreadyDefinedFunction( fun
                                                            , oc.remove() ) ),
          // In case there's already an implementation 
          // and this is just a declaration
          (&Some( _ ), &None) => {
            if oc.get().ty != fun.ty {
              return Err( HicrError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
          },
          // In case there's a declaration and this is a implementation
          (&None, &Some( _ )) => {
            if oc.get().ty != fun.ty {
              return Err( HicrError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
            oc.insert( fun );
          },
          // In case we have to delcarations
          (&None, &None) => {
            return Err( HicrError::AlreadyDeclaredFunction( fun
                                                          , oc.remove() ) ) 
          }
        }
      }
    }

    Ok( () )
  }

  fn resolve_namespaces( &mut self ) -> HResult<()> {
    // A list of all the new bindings that might be added
    let mut binds = Vec::new();

    for (fnam, fun) in self.functions.iter_mut() {
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
      -> HResult<()> {
    match expr {
      named @ &mut Expression::UnresolvedNamed( .. ) => {
        if let Expression::UnresolvedNamed( idt ) =
               replace( named, Expression::Invalid ) {
          if arguments.contains( &idt ) {
            *named = Expression::Arg( idt );
          } else if binds.iter()
                         .any( |&( ref n, _ )|
                            n.matches( scope, &idt.text[] ) ) {
            *named = Expression::Named( scope.ident_child( &idt ) );
          } else {
            *named = Expression::Named( Name::from_ident( &idt, None ) );
          }
        }
      },
      &mut Expression::Apply( ref mut exprs ) => {
        for e in exprs.iter_mut() {
          try!( Module::resolve_namespace_expr( e, arguments, scope, binds ) );
        }
      },
      &mut Expression::Literal( .. ) => {},
      &mut Expression::If( ref mut cond, ref mut thn, ref mut els ) => {
        // Enter the if conditon scope
        scope.push( "if".to_string() );
        try!( Module::resolve_namespace_expr( &mut **cond
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
      letexpr @ &mut Expression::Let( .. ) => {
        // Swap out the let expression from the AST
        if let Expression::Let( fns, bdy ) = replace( letexpr
                                                    , Expression::Invalid ) {
          // Scope the bindings and add them to the module
          for letfn in fns.into_iter() {
            binds.push( (scope.clone(), letfn) );
          }
         
          // Then replace this let node with it's body
          *letexpr = *bdy;
         
          // We enter the let body scope
          scope.push( "let".to_string() );
          
          // And resolve the body
          try!( Module::resolve_namespace_expr( letexpr
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

  fn validate_names( &self ) -> HResult<()> {

    for fun in self.functions.values() {
      if let Some( ref bdy ) = fun.body {
        try!( self.validate_name_expr( bdy ) )
      }
    }

    Ok( () )
  }

  fn validate_name_expr( &self, expr : &Expression ) -> HResult<()> {
    match expr {
      &Expression::Literal( .. ) => {},
      &Expression::Arg( .. ) => {}, // *BING* SKIP!
      &Expression::Apply( ref exprs ) => {
        for e in exprs.iter() {
          try!( self.validate_name_expr( e ) );
        }
      },
      &Expression::If( ref cond, ref thn, ref els ) => {
        try!( self.validate_name_expr( &**cond ) );
        try!( self.validate_name_expr( &**thn ) );
        try!( self.validate_name_expr( &**els ) );
      },
      &Expression::Named( ref name ) => {
        if name.is_toplevel() {

          if !self.functions.keys().any( |k| k.same( name ) ) {
            return Err( HicrError::UndefinedName( name.clone() ) )
          }
        }
      },
      inv => panic!( "Name validation is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

  fn resolve_applications( &mut self ) -> HResult<()> {
    Ok( () )
  }

  fn resolve_types( &mut self ) -> HResult<()> {
    Ok( () )
  }

  fn check_types( &mut self ) -> HResult<()> {
    Ok( () )
  }
}

pub fn validate_module( name : String, fns : Vec<Function> )
       -> HResult<Module> {
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


