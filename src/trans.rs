use std::collections::hash_map::{HashMap, Entry, Keys};
use std::mem::replace;

use ast::{Function, Type, Expression, Ident, Name, uexpr, Item, TypeDefinition};
use builtin::{BuiltinType, BuiltinFn, builtin_fn};
use folder::{self, Folder};
use visitor::Visitor;

// Code transformation and validation

// EK::* => ExpressionKind::*
#[allow(non_snake_case)]
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
  AlreadyDeclaredFunction( Function, Function ),
  AlreadyDefinedFunction( Function, Function ),
  FunctionTypeNotMatching( Function, Function ),
  InvalidArgumentList( Function ),

  AlreadyDefinedType( Name, Type, Type ),

  UndefinedName( Name ),

  IncosistentIfBranches( Expression ),
  IncorrectType( Expression, Type ),
  InvalidArgumentCount( Expression ),
  UncallableValue( Expression ),

}

pub type TResult<T> = Result<T, TransError>;

#[derive(Debug)]
pub struct Module {
  pub name      : String,
  pub functions : HashMap<Name, Function>,
  pub types     : HashMap<Name, Type>
  // data structures
}

impl Module {
  fn new( name : String ) -> Module {
    Module{ name: name, functions: HashMap::new(), types: HashMap::new() }
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

  fn insert_type_def( &mut self, tydef : TypeDefinition ) -> TResult<()> {
    let mut nam = Name::from_ident( tydef.name(), None );
    nam.no_loc();

    match self.types.entry( nam.clone() ) {
      Entry::Vacant( va ) => { va.insert( tydef.inner() ); },
      Entry::Occupied( mut oc ) =>
        return Err( TransError::AlreadyDefinedType( nam
                                                  , tydef.inner()
                                                  , oc.remove() ) )
    }

    Ok( () )
  }

  fn resolve_namespaces( &mut self ) -> TResult<()> {
    // A list of all the new bindings that might be added
    let mut binds = Vec::new();

    for (_, fun) in self.functions.iter_mut() {
      if let Some( ref mut body ) = fun.body {
        try!( Module::resolve_namespace_expr( body
                                            , &fun.arg_names
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
        // Enter the if condition scope
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
                                                  , &fun.arg_names
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

  /*
    TODO:
    "Abstract" function types
    Type functions ( generics )
    Partial application
    Environment capturing
  */

  fn check_types( &mut self ) -> TResult<()> {
    // Walk through the tree nodes, check for expectations and inconsistencies
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
            // Because the initial type of an if-expression is set to the type
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

        // Type check the arguments and callee
        try!( Module::type_check_expr( callee ) );

        for arg in args.iter_mut() {
          try!( Module::type_check_expr( arg ) );
        }

        try!( Module::type_check_function_call( callee, args, ety ) );

        for (i, arg) in args.iter_mut().enumerate() {
          try!( Module::coerce_expr( arg, callee.ty.argument_type( i ) ) );
        }

      },
      inv => panic!( "Type checking is not implemented for: {:?}", inv )
    }

    Ok( () )
  }

  fn type_check_function_call( callee : &mut Expression
                             , args   : &mut Vec<Expression>
                             , ety    : &mut Type ) -> TResult<()> {

    // Then make sure we're not trying to call a non-function
    if !callee.ty.is_fn_type() {
      // Just a bug test
      if args.len() == 0 {
        panic!( "Found a non-function call with zero arguments! {:?}"
              , callee )
      }
      // Report an error, we can't call a non-function value
      let errexpr = Expression::new( EK::FnCall( Box::new( callee.clone() )
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
        // panic!( "Over application is not implemented: {:?}", callee )

        // Notes on in-place modification:
        // - Currently we are inspecting the node where the over-application
        //   is initially found, which means if we want to expand in place
        //   this will become the outer node ( the one over-applying )

        // The leftover arguments are the ones remaining after we take
        // the ones needed for the root application
        
        // TYPES!
        // The root function call expression will have the type of it's
        // return value, while the current code will have the type of
        // the resulting application

        // Swap out the current nodes arguments for the leftover ones
        let left_over_args = args.split_off( callee.ty.argument_count() );
        let root_args = replace( args, left_over_args );

        // Temporarily plug out the root call
        let root_callee = replace( callee, uexpr( EK::Invalid ) );

        // Temporarily plug out the root type
        let root_ty = replace( ety, Type::Untyped );

        let root_call = Expression::new( EK::FnCall( Box::new( root_callee )
                                                   , root_args )
                                       , root_ty );

        // New the thing we're calling is the result of the last 
        *callee = root_call;

        // Since our previous if-statement check that the type of the function
        // call is another function, we can safely assume it also has a return
        // type. With that information we can set the outer calls initial type
        // to the return type of that ( it may be modified by the recursive
        // call of this function ).

        *ety = callee.ty.return_type().clone();

        // Do a recursive check on the newly generated node, so that
        // it's type check and so that chained over-application + partial
        // application is possible
        try!( Module::type_check_function_call( callee, args, ety ) )

      // Otherwise we assume that there's just too many arguments
      } else {
        let errexpr = Expression::new( EK::FnCall( Box::new( callee.clone() )
                                                 , args.clone() )
                                     , ety.clone() );
        return Err( TransError::InvalidArgumentCount( errexpr ) )
      }
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
  }

}

struct NameValidator<'a> {
  error : Option<TransError>,
  names : Keys<'a, Name, Function>
}

impl<'a> Visitor<'a> for NameValidator<'a> {
  fn visit_name( &mut self, name : &'a Name ) {
    let mut noloc_name = name.clone();
    noloc_name.no_loc();

    if !self.names.clone().any( |k| k.same( &noloc_name ) ) {
      self.error = Some( TransError::UndefinedName( name.clone() ) );
    }
  }

  fn is_done_visiting( &mut self ) -> bool {
    self.error.is_some()
  }
}

impl<'a> NameValidator<'a> {
  fn validate( module : &Module ) -> TResult<()> {
    let mut validator = NameValidator { error: None
                                      , names: module.functions.keys() };
    validator.visit_module( module );
    if let Some( err ) = validator.error {
      Err( err )
    } else {
      Ok( () )
    }
  }
}

struct TypeAnnotator {
  fntypes : HashMap<Name, Type>,
  argtypes : HashMap<Name, Type>
}

type IfEK = (Box<Expression>, Box<Expression>, Box<Expression>);

impl Folder for TypeAnnotator {
  type Failure = TransError;

  fn fold_fn( &mut self, mut fun : Function ) -> TResult<Function> {
    self.argtypes.clear();
    
    for (i, arg) in fun.arg_names.iter().enumerate() {
      let mut arg_key = Name::from_ident( arg, None );
      // Make the location consistent for the hash
      arg_key.no_loc();
      self.argtypes.insert( arg_key, fun.ty.argument_type( i ).clone() );
    }

    folder::follow_fn( fun, self )
  }

  fn fold_expr_if( &mut self, cte : IfEK, ty : Type ) -> TResult<(IfEK, Type)> {
    let ((cnd, thn, els), _) = try!( folder::follow_expr_if( cte, ty, self ) );
    let thnty = thn.ty.clone();
    Ok( ((cnd, thn, els), thnty) )
  }

  fn fold_expr_arg( &mut self, idt : Ident, ty : Type ) -> TResult<(Ident, Type)> {
    let (_, t) = self.get_type( Name::from_ident( &idt, None ) );
    Ok( (idt.clone(), t.expect( &format!( "Encountered non-existant arg. {:?}", idt ) ).clone()) )
  }

  fn fold_expr_named( &mut self, idt : Name, ty : Type ) -> TResult<(Name, Type)> {
    let (i, t) = self.get_type( idt.clone() );
    Ok( (i, t.expect( &format!( "Encountered non-existant fn. {:?}", idt ) ).clone()) )
  }

  fn fold_expr_builtin_fn( &mut self, bit : BuiltinFn, ty : Type ) -> TResult<(BuiltinFn, Type)> {
    let bity = bit.get_type();
    Ok( (bit, bity) )
  }

  fn fold_expr_fn_call( &mut self, f : (Box<Expression>, Vec<Expression>), ty : Type ) -> TResult<((Box<Expression>, Vec<Expression>), Type)> {
    let ((fe, fargs), _) = try!( folder::follow_expr_fn_call( f, ty, self ) );
    let ferety = fe.ty.return_type().clone();
    Ok( ((fe, fargs), ferety) )
  }
}

impl TypeAnnotator {
  fn annotate( module : Module ) -> TResult<Module> {
    let mut ftmap : HashMap<Name, Type> =
      module.functions.iter()
                      .map( |(k, v)| {
                        let mut nk = k.clone();
                        nk.no_loc();

                        (nk, v.ty.clone())
                      } )
                      .collect();
    
    let atmap = HashMap::new();

    TypeAnnotator { fntypes: ftmap, argtypes: atmap }.fold_module( module )
  }

  fn get_type( &mut self, mut nam : Name ) -> (Name, Option<Type>) {
    nam.no_loc();

    let rty = self.fntypes.get( &nam )
                          .or_else( || self.argtypes.get( &nam ) )
                          .map( |v| v.clone() );
    (nam, rty)
  }
}

// TODO: Add cyclic data structure detection
// TODO: Add Module::types to both folder and visitor

pub fn validate_module( name : String, items : Vec<Item> )
       -> TResult<Module> {
  let mut module = Module::new( name );
  
  for item in items.into_iter() {
    match item {
      Item::Fn( fun ) => try!( module.insert_toplevel_fn( fun ) ),
      Item::Type( ty ) => try!( module.insert_type_def( ty ) )
    }
  }

  try!( module.resolve_namespaces() );
  try!( NameValidator::validate( &module ) );
  try!( module.resolve_applications() );
  // try!( module.resolve_types() );
  module = try!( TypeAnnotator::annotate( module ) );
  try!( module.check_types() );
  
  Ok( module )
}


