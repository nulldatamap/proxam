use std::ptr;
use std::mem::replace;
use std::collections::hash_map::Entry;

use ast::{Type, Expression, ExpressionKind, Class, Name
         , Ident, Function, Literal, uexpr};
use builtin::{BuiltinType, BuiltinFn};
use trans::Module;

/*
  This is a spin on the visitor pattern, just like visitor it
  visits each node of the AST, but instead of just visiting it also
  takes out the value from the tree, enabling us to tranform it, and 
  then puts it back. This enables us to modify the tree in a type-safe way

*/

#[allow(non_snake_case)]
mod EK {
  pub use ast::ExpressionKind::*;
}

pub trait Invalidable {
  fn invalid() -> Self;
}

impl Invalidable for Type {
  fn invalid() -> Type {
    Type::Untyped
  }
}

impl Invalidable for Expression {
  fn invalid() -> Expression {
    uexpr( ExpressionKind::Invalid )
  }
}

pub trait MapTrying<V, E> {
  fn try_map<F>( mut self, mut f : F ) -> Result<Self, E> where F : FnMut( V ) -> Result<V, E>;
}

impl<T, E> MapTrying<T, E> for Vec<T> {
  fn try_map<F>( mut self, mut f : F ) -> Result<Vec<T>, E>
    where F: FnMut( T ) -> Result<T, E> {

    for elm in self.iter_mut() {
      unsafe {
        ptr::write( elm, try!( f( ptr::read( elm ) ) ) );
      }
    }
    Ok( self )
  }
}

impl<T, E> MapTrying<T, E> for Option<T> {
  fn try_map<F>( mut self, mut f : F ) -> Result<Option<T>, E>
    where F: FnMut( T ) -> Result<T, E> {

    if let Some( v ) = self {
      self = Some( try!( f( v ) ) )
    }

    Ok( self )
  }
}

impl<T, E> MapTrying<T, E> for Box<T>
  where T: Invalidable {

  fn try_map<F>( mut self, mut f : F ) -> Result<Box<T>, E>
    where F: FnMut( T ) -> Result<T, E> {

    let mut inner = try!( f( replace( &mut self, Invalidable::invalid() ) ) );
    
    replace( &mut *self, inner );
    
    Ok( self )
  }
}

pub trait Folder : Sized {
  type Failure;

  fn fold_module( &mut self, v : Module ) -> Result<Module, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_module( v, self )
  }

  fn fold_fn( &mut self, v : Function ) -> Result<Function, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_fn( v, self )
  }

  fn fold_typedef( &mut self, v : (&Name, Type) ) -> Result<Type, Self::Failure> {
    if self.is_done_folding() { return Ok( v.1 ) }
    follow_typedef( v, self )
  }

  fn fold_ident( &mut self, v : Ident ) -> Result<Ident, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ident( v, self )
  }

  fn fold_name( &mut self, v : Name ) -> Result<Name, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_name( v, self )
  }

  fn fold_ty( &mut self, v : Type ) -> Result<Type, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty( v, self )
  }

  fn fold_tys( &mut self, v : Vec<Type> ) -> Result<Vec<Type>, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_tys( v, self )
  }

  fn fold_ty_named_type( &mut self, v : Ident ) -> Result<Ident, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_named_type( v, self )
  }

  fn fold_ty_unit( &mut self ) -> Result<(), Self::Failure> {
    if self.is_done_folding() { return Ok( () ) }
    follow_ty_unit( self )
  }

  fn fold_ty_tuple( &mut self, v : Vec<Type> ) -> Result<Vec<Type>, Self::Failure> {
    follow_ty_tuple( v, self )
  }

  fn fold_ty_list( &mut self, v : Box<Type> ) -> Result<Box<Type>, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_list( v, self )
  }

  fn fold_ty_fn( &mut self, v : (Vec<Type>, Box<Type>) ) -> Result<(Vec<Type>, Box<Type>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_fn( v, self )
  }

  fn fold_ty_generic( &mut self, v : (Ident, Vec<Ident>) ) -> Result<(Ident, Vec<Ident>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_generic( v, self )
  }

  fn fold_ty_abstract_fn( &mut self, v : (Vec<Type>, Box<Type>) ) -> Result<(Vec<Type>, Box<Type>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_abstract_fn( v, self )
  }

  fn fold_ty_closure( &mut self, v : (Vec<Type>, Vec<Type>, Box<Type>) ) -> Result<(Vec<Type>, Vec<Type>, Box<Type>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_closure( v, self )
  }

  fn fold_ty_builtin_type( &mut self, v : BuiltinType ) -> Result<BuiltinType, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_builtin_type( v, self )
  }

  fn fold_ty_application( &mut self, v : (Box<Type>, u32) ) -> Result<(Box<Type>, u32), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_application( v, self )
  }

  fn fold_ty_unique( &mut self, v : (Ident, Box<Type>) ) -> Result<(Ident, Box<Type>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_unique( v, self )
  }

  fn fold_ty_structure( &mut self, v : Vec<(Ident, Type)> ) -> Result<Vec<(Ident, Type)>, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_ty_structure( v, self )
  }

  fn fold_ty_untyped( &mut self ) -> Result<(), Self::Failure> {
    if self.is_done_folding() { return Ok( () ) }
    follow_ty_untyped( self )
  }

  fn fold_class( &mut self, v : Class ) -> Result<Class, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_class( v, self )
  }

  fn fold_expr( &mut self, v : Expression ) -> Result<Expression, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr( v, self )
  }

  fn fold_exprs( &mut self, v : Vec<Expression> ) -> Result<Vec<Expression>, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_exprs( v, self )
  }

  fn fold_expr_let( &mut self, v : (Vec<Function>, Box<Expression>), ty : Type ) -> Result<((Vec<Function>, Box<Expression>), Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_let( v, ty, self )
  }

  fn fold_expr_unresolved_name( &mut self, v : Ident, ty : Type ) -> Result<(Ident, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_unresolved_name( v, ty, self )
  }

  fn fold_expr_apply( &mut self, v : Vec<Expression>, ty : Type ) -> Result<(Vec<Expression>, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_apply( v, ty, self )
  }

  fn fold_expr_if( &mut self, v : (Box<Expression>, Box<Expression>, Box<Expression>), ty : Type ) -> Result<((Box<Expression>, Box<Expression>, Box<Expression>), Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_if( v, ty, self )
  }

  fn fold_expr_literal( &mut self, v : Literal, ty : Type ) -> Result<(Literal, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_literal( v, ty, self )
  }

  fn fold_expr_arg( &mut self, v : Ident, ty : Type ) -> Result<(Ident, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_arg( v, ty, self )
  }

  fn fold_expr_named( &mut self, v : Name, ty : Type ) -> Result<(Name, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_named( v, ty, self )
  }

  fn fold_expr_builtin_fn( &mut self, v : BuiltinFn, ty : Type ) -> Result<(BuiltinFn, Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_builtin_fn( v, ty, self )
  }

  fn fold_expr_fn_call( &mut self, v : (Box<Expression>, Vec<Expression>), ty : Type ) -> Result<((Box<Expression>, Vec<Expression>), Type), Self::Failure> {
    if self.is_done_folding() { return Ok( (v, ty) ) }
    follow_expr_fn_call( v, ty, self )
  }

  fn fold_expr_invalid( &mut self, ty : Type ) -> Result<Type, Self::Failure> {
    if self.is_done_folding() { return Ok( ty ) }
    follow_expr_invalid( ty, self )
  }

  fn fold_literal( &mut self, v : Literal ) -> Result<Literal, Self::Failure> {
    Ok( v )
  }

  fn fold_builtin_fn( &mut self, v : BuiltinFn ) -> Result<BuiltinFn, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_builtin_fn( v, self )
  }

  fn is_done_folding( &mut self ) -> bool {
    false
  }
}

pub fn follow_module<F : Folder>( mut v : Module, folder : &mut F ) -> Result<Module, <F as Folder>::Failure> {
  for (n, t) in v.types.iter_mut() {
    let ty = replace( t, Type::Untyped );
    *t = try!( folder.fold_typedef( (n, ty) ) );
  }

  for (_, f) in v.functions.iter_mut() {
    let fun = replace( f, Function::empty() );
    *f = try!( folder.fold_fn( fun ) );
  }

  Ok( v )
}

pub fn follow_expr_invalid<F : Folder>( ty : Type, folder : &mut F ) -> Result<Type, <F as Folder>::Failure> {
  Ok( try!( folder.fold_ty( ty ) ) )
}

 
pub fn follow_ident<F : Folder>( v : Ident, folder : &mut F ) -> Result<Ident, <F as Folder>::Failure> {
  Ok( v )
}

 
pub fn follow_ty_unit<F : Folder>( folder : &mut F ) -> Result<(), <F as Folder>::Failure> {
  Ok( () )
}

 
pub fn follow_ty_untyped<F : Folder>( folder : &mut F ) -> Result<(), <F as Folder>::Failure> {
  Ok( () )
}

 
pub fn follow_fn<F : Folder>( mut v : Function, folder : &mut F ) -> Result<Function, <F as Folder>::Failure> {
  v.name = try!( folder.fold_ident( v.name ) );
  v.ty   = try!( folder.fold_ty( v.ty ) );
  v.arg_names = try!( v.arg_names.try_map( |a| folder.fold_ident( a ) ) );
  // v.body = try!( v.body.try_map( |b| folder.fold_expr( b ) ) );

  v.body = if let Some( bdy ) = v.body {
    Some( try!( folder.fold_expr( bdy ) ) )
  } else { None };

  v.constraints = try!( v.constraints.try_map( |c| folder.fold_class( c ) ) );
  Ok( v )
}

pub fn follow_typedef<F : Folder>( (n, t) : (&Name, Type), folder : &mut F ) -> Result<Type, <F as Folder>::Failure> {
  folder.fold_ty( t )
}


pub fn follow_name<F : Folder>( v : Name, folder : &mut F ) -> Result<Name, <F as Folder>::Failure> {
  Ok( v )
}


pub fn follow_ty<F : Folder>( v : Type, folder : &mut F ) -> Result<Type, <F as Folder>::Failure> {
  Ok( match v {
    Type::NamedType( idt ) => Type::NamedType( try!( folder.fold_ty_named_type( idt ) ) ),
    Type::Unit => {
      try!( folder.fold_ty_unit() );
      Type::Unit
    },
    Type::Unique( id, ty ) => {
      let (i, t) = try!( folder.fold_ty_unique( (id, ty) ) );
      Type::Unique( i, t )
    },
    Type::Structure( pairs ) => {
      Type::Structure( try!( folder.fold_ty_structure( pairs ) ) )
    },
    Type::Tuple( ts ) => Type::Tuple( try!( folder.fold_ty_tuple( ts ) ) ),
    Type::List( inner ) => Type::List( try!( folder.fold_ty_list( inner ) ) ),
    Type::Fn( args, ret ) => {
        let (a, r) = try!( folder.fold_ty_fn( (args, ret) ) );
        Type::Fn( a, r )
      },
    Type::Generic( idt, idts ) => {
        let (id, ids) = try!( folder.fold_ty_generic( (idt, idts) ) );
        Type::Generic( id, ids )
      },
    Type::AbstractFn( args, ret ) => {
        let (a, r) = try!( folder.fold_ty_abstract_fn( (args, ret) ) );
        Type::AbstractFn( a, r )
      },
    Type::Closure( env, args, ret ) => {
        let (e, a, r) = try!( folder.fold_ty_closure( (env, args, ret) ) );
        Type::Closure( e, a, r )
      }
    Type::BuiltinType( bit ) => Type::BuiltinType( try!( folder.fold_ty_builtin_type( bit ) ) ),
    Type::Application( inner, aps ) => {
        let (i, a) = try!( folder.fold_ty_application( (inner, aps) ) );
        Type::Application( i, a )  
      },
    Type::Untyped => {
      try!( folder.fold_ty_untyped() );
      Type::Untyped
    }
  } )
}


pub fn follow_tys<F : Folder>( v : Vec<Type>, folder : &mut F ) -> Result<Vec<Type>, <F as Folder>::Failure> {
  v.try_map( |e| folder.fold_ty( e ) )
}


pub fn follow_ty_named_type<F : Folder>( v : Ident, folder : &mut F ) -> Result<Ident, <F as Folder>::Failure> {
  folder.fold_ident( v )
}


pub fn follow_ty_tuple<F : Folder>( v : Vec<Type>, folder : &mut F ) -> Result<Vec<Type>, <F as Folder>::Failure> {
  folder.fold_tys( v )
}


pub fn follow_ty_list<F : Folder>( v : Box<Type>, folder : &mut F ) -> Result<Box<Type>, <F as Folder>::Failure> {
  v.try_map( |t| folder.fold_ty( t ) )
}


pub fn follow_ty_fn<F : Folder>( (args, ret) : (Vec<Type>, Box<Type>), folder : &mut F ) -> Result<(Vec<Type>, Box<Type>), <F as Folder>::Failure> {
  let a = try!( folder.fold_tys( args ) );
  let r = try!( ret.try_map( |r| folder.fold_ty( r ) ) );

  Ok( (a, r) )
}


pub fn follow_ty_generic<F : Folder>( (idt, idts) : (Ident, Vec<Ident>), folder : &mut F ) -> Result<(Ident, Vec<Ident>), <F as Folder>::Failure> {
  let id = try!( folder.fold_ident( idt ) );
  let ids = try!( idts.try_map( |i| folder.fold_ident( i ) ) );

  Ok( (id, ids) )
}


pub fn follow_ty_abstract_fn<F : Folder>( (args, ret) : (Vec<Type>, Box<Type>), folder : &mut F ) -> Result<(Vec<Type>, Box<Type>), <F as Folder>::Failure> {
  let a = try!( folder.fold_tys( args ) );
  let r = try!( ret.try_map( |r| folder.fold_ty( r ) ) );
  Ok( (a, r) )
}


pub fn follow_ty_closure<F : Folder>( (env, args, ret) : (Vec<Type>, Vec<Type>, Box<Type>), folder : &mut F ) -> Result<(Vec<Type>, Vec<Type>, Box<Type>), <F as Folder>::Failure> {
  let e = try!( folder.fold_tys( env ) );
  let a = try!( folder.fold_tys( args ) );
  let r = try!( ret.try_map( |r| folder.fold_ty( r ) ) );

  Ok( (e, a, r) )
}


pub fn follow_ty_builtin_type<F : Folder>( v : BuiltinType, folder : &mut F ) -> Result<BuiltinType, <F as Folder>::Failure> {
  Ok( v )
}


pub fn follow_ty_application<F : Folder>( (inner, aps) : (Box<Type>, u32), folder : &mut F ) -> Result<(Box<Type>, u32), <F as Folder>::Failure> {
  let i = try!( inner.try_map( |t| folder.fold_ty( t ) ) );
  Ok( (i, aps) )
}

pub fn follow_ty_unique<F : Folder>( (id, ty) : (Ident, Box<Type>), folder : &mut F ) -> Result<(Ident, Box<Type>), <F as Folder>::Failure> {
  let i = try!( folder.fold_ident( id ) );
  let t = try!( ty.try_map( |v| folder.fold_ty( v ) ) );

  Ok( (i, t) )
}

pub fn follow_ty_structure<F : Folder>( pairs : Vec<(Ident, Type)>, folder : &mut F ) -> Result<Vec<(Ident, Type)>, <F as Folder>::Failure> {
  let p = try!( pairs.try_map( |(id, ty)| {
    let i = try!( folder.fold_ident( id ) );
    let t = try!( folder.fold_ty( ty ) );
    Ok( (i, t) )
  } ) );

  Ok( p )
}

pub fn follow_class<F : Folder>( mut v : Class, folder : &mut F ) -> Result<Class, <F as Folder>::Failure> {
  v.name = try!( folder.fold_ident( v.name ) );
  v.params = try!( folder.fold_tys( v.params ) );

  Ok( v )
}


pub fn follow_expr<F : Folder>( mut v : Expression, folder : &mut F ) -> Result<Expression, <F as Folder>::Failure> {
  let Expression { mut kind, mut ty } = v;
  kind = match kind {
    EK::Let( fs, bdy ) => {
        let ((f, b), t) = try!( folder.fold_expr_let( (fs, bdy), ty ) );
        ty = t;
        EK::Let( f, b )
      },
    EK::UnresolvedNamed( idt ) => {
        let (v, t) = try!( folder.fold_expr_unresolved_name( idt, ty ) );
        ty = t;
        EK::UnresolvedNamed( v )
      },
    EK::Apply( exprs ) => {
        let (v, t) = try!( folder.fold_expr_apply( exprs, ty ) );
        ty = t;
        EK::Apply( v )
      },
    EK::If( cnd, thn, els ) => {
        let ((c, th, e), t) = try!( folder.fold_expr_if( (cnd, thn, els), ty ) );
        ty = t;
        EK::If( c, th, e )
      },
    EK::Literal( lit ) => {
        let (v, t) = try!( folder.fold_expr_literal( lit, ty ) );
        ty = t;
        EK::Literal( v )
      },
    EK::Arg( idt ) => {
      let (v, t) = try!( folder.fold_expr_arg( idt, ty ) );
      ty = t;
      EK::Arg( v )
    },
    EK::Named( nam ) => {
      let (v, t) = try!( folder.fold_expr_named( nam, ty ) );
      ty = t; 
      EK::Named( v )
    },
    EK::BuiltinFn( bif ) => {
      let (v, t) = try!( folder.fold_expr_builtin_fn( bif, ty ) );
      ty = t;
      EK::BuiltinFn( v )
    },
    EK::FnCall( callee, args ) => {
        let ((c, a), t) = try!( folder.fold_expr_fn_call( (callee, args), ty ) );
        ty = t;
        EK::FnCall( c, a )   
      },
    EK::Invalid => {
        ty = try!( folder.fold_expr_invalid( ty ) );
        EK::Invalid
      },
  };
  Ok( Expression { kind: kind, ty: ty } )
}


pub fn follow_exprs<F : Folder>( v : Vec<Expression>, folder : &mut F ) -> Result<Vec<Expression>, <F as Folder>::Failure> {
  v.try_map( |e| folder.fold_expr( e ) )
}


pub fn follow_expr_let<F : Folder>( (fs, ex) : (Vec<Function>, Box<Expression>), ty : Type, folder : &mut F ) -> Result<((Vec<Function>, Box<Expression>), Type), <F as Folder>::Failure> {
  Ok( ((try!( fs.try_map( |f| folder.fold_fn( f ) ) )
        , try!( ex.try_map( |e| folder.fold_expr( e ) ) ) )
      , try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_unresolved_name<F : Folder>( v : Ident, ty : Type, folder : &mut F ) -> Result<(Ident, Type), <F as Folder>::Failure> {
  Ok( (try!( folder.fold_ident( v ) ), try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_apply<F : Folder>( v : Vec<Expression>, ty : Type, folder : &mut F ) -> Result<(Vec<Expression>, Type), <F as Folder>::Failure> {
  Ok( (try!( v.try_map( |val| folder.fold_expr( val ) ) )
      , try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_if<F : Folder>( (cnd, thn, els) : (Box<Expression>, Box<Expression>, Box<Expression>), ty : Type, folder : &mut F ) -> Result<((Box<Expression>, Box<Expression>, Box<Expression>), Type), <F as Folder>::Failure> {
  Ok( ((try!( cnd.try_map( |c| folder.fold_expr( c ) ) )
        , try!( thn.try_map( |t| folder.fold_expr( t ) ) )
        , try!( els.try_map( |e| folder.fold_expr( e ) ) ) )
       , try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_literal<F : Folder>( v : Literal, ty : Type, folder : &mut F ) -> Result<(Literal, Type), <F as Folder>::Failure> {
  Ok( (try!( folder.fold_literal( v ) ), try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_arg<F : Folder>( v : Ident, ty : Type, folder : &mut F ) -> Result<(Ident, Type), <F as Folder>::Failure> {
  Ok( (try!( folder.fold_ident( v ) ), try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_named<F : Folder>( v : Name, ty : Type, folder : &mut F ) -> Result<(Name, Type), <F as Folder>::Failure> {
  Ok( ( try!( folder.fold_name( v ) ), try!( folder.fold_ty( ty ) ) ) )
}


pub fn follow_expr_builtin_fn<F : Folder>( v : BuiltinFn, ty : Type, folder : &mut F ) -> Result<(BuiltinFn, Type), <F as Folder>::Failure> {
  Ok( (try!( folder.fold_builtin_fn( v ) ), try!( folder.fold_ty( ty ) )) )
}


pub fn follow_expr_fn_call<F : Folder>( (cl, ar) : (Box<Expression>, Vec<Expression>), ty : Type, folder : &mut F ) -> Result<((Box<Expression>, Vec<Expression>), Type), <F as Folder>::Failure> {
  Ok( ((try!( cl.try_map( |c| folder.fold_expr( c ) ) )
        , try!( folder.fold_exprs( ar ) ))
      , try!( folder.fold_ty( ty ) )) )
}


pub fn follow_builtin_fn<F : Folder>( v : BuiltinFn, folder : &mut F ) -> Result<BuiltinFn, <F as Folder>::Failure> {
  Ok( v )
}

