use std::ptr;
use std::mem::replace;

use ast::{Type, Expression, ExpressionKind, Class, Name
         , Ident, Function, Literal, uexpr};
use builtin::{BuiltinType, BuiltinFn};

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

  fn fold_fn( &mut self, v : Function ) -> Result<Function, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_fn( v, self )
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

  fn fold_expr_kind( &mut self, v : ExpressionKind ) -> Result<ExpressionKind, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind( v, self )
  }

  fn fold_expr_kind_let( &mut self, v : (Vec<Function>, Box<Expression>) ) -> Result<(Vec<Function>, Box<Expression>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_let( v, self )
  }

  fn fold_expr_kind_unresolved_name( &mut self, v : Ident ) -> Result<Ident, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_unresolved_name( v, self )
  }

  fn fold_expr_kind_apply( &mut self, v : Vec<Expression> ) -> Result<Vec<Expression>, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_apply( v, self )
  }

  fn fold_expr_kind_if( &mut self, v : (Box<Expression>, Box<Expression>, Box<Expression>) ) -> Result<(Box<Expression>, Box<Expression>, Box<Expression>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_if( v, self )
  }

  fn fold_expr_kind_literal( &mut self, v : Literal ) -> Result<Literal, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_literal( v, self )
  }

  fn fold_expr_kind_arg( &mut self, v : Ident ) -> Result<Ident, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_arg( v, self )
  }

  fn fold_expr_kind_named( &mut self, v : Name ) -> Result<Name, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_named( v, self )
  }

  fn fold_expr_kind_builtin_fn( &mut self, v : BuiltinFn ) -> Result<BuiltinFn, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_builtin_fn( v, self )
  }

  fn fold_expr_kind_fn_call( &mut self, v : (Box<Expression>, Vec<Expression>) ) -> Result<(Box<Expression>, Vec<Expression>), Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_expr_kind_fn_call( v, self )
  }

  fn fold_expr_kind_invalid( &mut self ) -> Result<(), Self::Failure> {
    if self.is_done_folding() { return Ok( () ) }
    follow_expr_kind_invalid( self )
  }

  fn fold_literal( &mut self, v : Literal ) -> Result<Literal, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_literal( v, self )
  }

  fn fold_builtin_fn( &mut self, v : BuiltinFn ) -> Result<BuiltinFn, Self::Failure> {
    if self.is_done_folding() { return Ok( v ) }
    follow_builtin_fn( v, self )
  }

  fn is_done_folding( &mut self ) -> bool {
    false
  }
}

pub fn follow_expr_kind_invalid<F : Folder>( folder : &mut F ) -> Result<(), <F as Folder>::Failure> {
  Ok( () )
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
  v.body = try!( v.body.try_map( |b| folder.fold_expr( b ) ) );
  v.constraints = try!( v.constraints.try_map( |c| folder.fold_class( c ) ) );
  Ok( v )
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
      folder.fold_ty_untyped();
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


pub fn follow_class<F : Folder>( mut v : Class, folder : &mut F ) -> Result<Class, <F as Folder>::Failure> {
  v.name = try!( folder.fold_ident( v.name ) );
  v.params = try!( folder.fold_tys( v.params ) );

  Ok( v )
}


pub fn follow_expr<F : Folder>( mut v : Expression, folder : &mut F ) -> Result<Expression, <F as Folder>::Failure> {
  v.kind = try!( folder.fold_expr_kind( v.kind ) );
  v.ty = try!( folder.fold_ty( v.ty ) );
  
  Ok( v )
}


pub fn follow_exprs<F : Folder>( v : Vec<Expression>, folder : &mut F ) -> Result<Vec<Expression>, <F as Folder>::Failure> {
  v.try_map( |e| folder.fold_expr( e ) )
}


pub fn follow_expr_kind<F : Folder>( v : ExpressionKind, folder : &mut F ) -> Result<ExpressionKind, <F as Folder>::Failure> {
  Ok( match v {
    EK::Let( fs, bdy ) => {
        let (f, b) = try!( folder.fold_expr_kind_let( (fs, bdy) ) );
        EK::Let( f, b )
      },
    EK::UnresolvedNamed( idt ) =>
      EK::UnresolvedNamed( try!( folder.fold_expr_kind_unresolved_name( idt ) ) ),
    EK::Apply( exprs ) =>
      EK::Apply( try!( folder.fold_expr_kind_apply( exprs ) ) ),
    EK::If( cnd, thn, els ) => {
        let (c, t, e) = try!( folder.fold_expr_kind_if( (cnd, thn, els) ) );
        EK::If( c, t, e )
      },
    EK::Literal( lit ) =>
      EK::Literal( try!( folder.fold_expr_kind_literal( lit ) ) ),
    EK::Arg( idt ) =>
      EK::Arg( try!( folder.fold_expr_kind_arg( idt ) ) ),
    EK::Named( nam ) =>
      EK::Named( try!( folder.fold_expr_kind_named( nam ) ) ),
    EK::BuiltinFn( bif ) =>
      EK::BuiltinFn( try!( folder.fold_expr_kind_builtin_fn( bif ) ) ),
    EK::FnCall( callee, args ) => {
        let (c, a) = try!( folder.fold_expr_kind_fn_call( (callee, args) ) );
        EK::FnCall( c, a )   
      },
    EK::Invalid => {
        try!( folder.fold_expr_kind_invalid() );
        EK::Invalid
      },
  } )
}


pub fn follow_expr_kind_let<F : Folder>( (fs, ex) : (Vec<Function>, Box<Expression>), folder : &mut F ) -> Result<(Vec<Function>, Box<Expression>), <F as Folder>::Failure> {
  Ok( (try!( fs.try_map( |f| folder.fold_fn( f ) ) )
      , try!( ex.try_map( |e| folder.fold_expr( e ) ) ) ) )
}


pub fn follow_expr_kind_unresolved_name<F : Folder>( v : Ident, folder : &mut F ) -> Result<Ident, <F as Folder>::Failure> {
  Ok( try!( folder.fold_ident( v ) ) )
}


pub fn follow_expr_kind_apply<F : Folder>( v : Vec<Expression>, folder : &mut F ) -> Result<Vec<Expression>, <F as Folder>::Failure> {
  Ok( try!( v.try_map( |val| folder.fold_expr( val ) ) ) )
}


pub fn follow_expr_kind_if<F : Folder>( (cnd, thn, els) : (Box<Expression>, Box<Expression>, Box<Expression>), folder : &mut F ) -> Result<(Box<Expression>, Box<Expression>, Box<Expression>), <F as Folder>::Failure> {
  Ok( (try!( cnd.try_map( |c| folder.fold_expr( c ) ) )
      , try!( thn.try_map( |t| folder.fold_expr( t ) ) )
      , try!( els.try_map( |e| folder.fold_expr( e ) ) ) ) )
}


pub fn follow_expr_kind_literal<F : Folder>( v : Literal, folder : &mut F ) -> Result<Literal, <F as Folder>::Failure> {
  Ok( try!( folder.fold_literal( v ) ) )
}


pub fn follow_expr_kind_arg<F : Folder>( v : Ident, folder : &mut F ) -> Result<Ident, <F as Folder>::Failure> {
  Ok( try!( folder.fold_ident( v ) ) )
}


pub fn follow_expr_kind_named<F : Folder>( v : Name, folder : &mut F ) -> Result<Name, <F as Folder>::Failure> {
  Ok( try!( folder.fold_name( v ) ) )
}


pub fn follow_expr_kind_builtin_fn<F : Folder>( v : BuiltinFn, folder : &mut F ) -> Result<BuiltinFn, <F as Folder>::Failure> {
  Ok( try!( folder.fold_builtin_fn( v ) ) )
}


pub fn follow_expr_kind_fn_call<F : Folder>( (cl, ar) : (Box<Expression>, Vec<Expression>), folder : &mut F ) -> Result<(Box<Expression>, Vec<Expression>), <F as Folder>::Failure> {
  Ok( (try!( cl.try_map( |c| folder.fold_expr( c ) ) )
      , try!( folder.fold_exprs( ar ) )) )
}


pub fn follow_literal<F : Folder>( v : Literal, folder : &mut F ) -> Result<Literal, <F as Folder>::Failure> {
  Ok( try!( folder.fold_literal( v ) ) )
}

pub fn follow_builtin_fn<F : Folder>( v : BuiltinFn, folder : &mut F ) -> Result<BuiltinFn, <F as Folder>::Failure> {
  Ok( v )
}

