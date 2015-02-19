use ast::{Type, Expression, ExpressionKind, Class, Name
         , Ident, Function, Literal, uexpr};
use builtin::{BuiltinType, BuiltinFn};

mod EK {
  pub use ast::ExpressionKind::*;
}

pub trait Visitor<'a> : Sized {

  fn visit_fn( &mut self, v : &'a Function ) {
    if self.is_done_visiting() { return }
    walk_fn( v, self )
  }

  fn visit_ident( &mut self, v : &'a Ident ) {
  }

  fn visit_name( &mut self, v : &'a Name ) {
  }

  fn visit_ty( &mut self, v : &'a Type ) {
    if self.is_done_visiting() { return }
    walk_ty( v, self )
  }

  fn visit_tys( &mut self, v : &'a [Type] ) {
    if self.is_done_visiting() { return }
    walk_tys( v, self )
  }

  fn visit_ty_named_type( &mut self, v : &'a Ident ) {
    if self.is_done_visiting() { return }
    walk_ty_named_type( v, self )
  }

  fn visit_ty_unit( &mut self ) {
  }

  fn visit_ty_tuple( &mut self, v : &'a [Type] ) {
    if self.is_done_visiting() { return }
    walk_ty_tuple( v, self )
  }

  fn visit_ty_list( &mut self, v : &'a Type ) {
    if self.is_done_visiting() { return }
    walk_ty_list( v, self )
  }

  fn visit_ty_fn( &mut self, v : (&'a [Type], &'a Type) ) {
    if self.is_done_visiting() { return }
    walk_ty_fn( v, self )
  }

  fn visit_ty_generic( &mut self, v : (&'a Ident, &'a [Ident]) ) {
    if self.is_done_visiting() { return }
    walk_ty_generic( v, self )
  }

  fn visit_ty_abstract_fn( &mut self, v : (&'a [Type], &'a Type) ) {
    if self.is_done_visiting() { return }
    walk_ty_abstract_fn( v, self )
  }

  fn visit_ty_closure( &mut self, v : (&'a [Type], &'a [Type], &'a Type) ) {
    if self.is_done_visiting() { return }
    walk_ty_closure( v, self )
  }

  fn visit_ty_builtin_type( &mut self, v : &'a BuiltinType ) {
  }

  fn visit_ty_application( &mut self, v : (&'a Type, u32) ) {
    if self.is_done_visiting() { return }
    walk_ty_application( v, self )
  }

  fn visit_ty_untyped( &mut self ) {
  }


  fn visit_class( &mut self, v : &'a Class ) {
    if self.is_done_visiting() { return }
    walk_class( v, self )
  }

  fn visit_expr( &mut self, v : &'a Expression ) {
    if self.is_done_visiting() { return }
    walk_expr( v, self )
  }

  fn visit_exprs( &mut self, v : &'a [Expression] ) {
    if self.is_done_visiting() { return }
    walk_exprs( v, self )
  }

  fn visit_expr_kind( &mut self, v : &'a ExpressionKind ) {
    if self.is_done_visiting() { return }
    walk_expr_kind( v, self )
  }

  fn visit_expr_kind_let( &mut self, v : (&'a [Function], &'a Expression) ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_let( v, self )
  }

  fn visit_expr_kind_unresolved_name( &mut self, v : &'a Ident ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_unresolved_name( v, self )
  }

  fn visit_expr_kind_apply( &mut self, v : &'a [Expression] ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_apply( v, self )
  }

  fn visit_expr_kind_if( &mut self, v : (&'a Expression, &'a Expression, &'a Expression) ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_if( v, self )
  }

  fn visit_expr_kind_literal( &mut self, v : &'a Literal ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_literal( v, self )
  }

  fn visit_expr_kind_arg( &mut self, v : &'a Ident ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_arg( v, self )
  }

  fn visit_expr_kind_named( &mut self, v : &'a Name ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_named( v, self )
  }

  fn visit_expr_kind_builtin_fn( &mut self, v : &'a BuiltinFn ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_builtin_fn( v, self )
  }

  fn visit_expr_kind_fn_call( &mut self, v : (&'a Expression, &'a [Expression]) ) {
    if self.is_done_visiting() { return }
    walk_expr_kind_fn_call( v, self )
  }

  fn visit_expr_kind_invalid( &mut self ) {
  }


  fn visit_literal( &mut self, v : &'a Literal ) {
  }

  fn visit_builtin_fn( &mut self, v : &'a BuiltinFn ) {
  }

  fn is_done_visiting( &mut self ) -> bool {
    false
  }

}


pub fn walk_fn<'a, V : Visitor<'a>>( v : &'a Function, visitor : &mut V ) {
  visitor.visit_ident( &v.name );
  visitor.visit_ty( &v.ty );
  v.arg_names.iter().map( |arg| visitor.visit_ident( arg ) );
  if let Some( ref bdy ) = v.body {
    visitor.visit_expr( bdy );
  }
  v.constraints.iter().map( |cls| visitor.visit_class( cls ) );
}

pub fn walk_ty<'a, V : Visitor<'a>>( v : &'a Type, visitor : &mut V ) {
  match v {
    &Type::NamedType( ref idt ) => visitor.visit_ty_named_type( idt ),
    &Type::Unit => visitor.visit_ty_unit(),
    &Type::Tuple( ref ts ) => visitor.visit_ty_tuple( ts ),
    &Type::List( ref t ) => visitor.visit_ty_list( t ),
    &Type::Fn( ref ar, ref re ) => visitor.visit_ty_fn( (ar, re) ),
    &Type::Generic( ref id, ref ids ) => visitor.visit_ty_generic( (id, ids) ),
    &Type::AbstractFn( ref ar, ref re ) => visitor.visit_ty_abstract_fn( (ar, re) ),
    &Type::Closure( ref en, ref ar, ref re ) => visitor.visit_ty_closure( (en, ar, re) ),
    &Type::BuiltinType( ref bit ) => visitor.visit_ty_builtin_type( bit ),
    &Type::Application( ref innr, ref ap ) => visitor.visit_ty_application( (innr, *ap) ),
    &Type::Untyped => visitor.visit_ty_untyped(),
  }
}

pub fn walk_tys<'a, V : Visitor<'a>>( v : &'a [Type], visitor : &mut V ) {
  v.iter().map( |elm| visitor.visit_ty( elm ) );
}

pub fn walk_ty_named_type<'a, V : Visitor<'a>>( v : &'a Ident, visitor : &mut V ) {
  visitor.visit_ty_named_type( v )
}


pub fn walk_ty_tuple<'a, V : Visitor<'a>>( v : &'a [Type], visitor : &mut V ) {
  visitor.visit_ty_tuple( v )
}

pub fn walk_ty_list<'a, V : Visitor<'a>>( v : &'a Type, visitor : &mut V ) {
  visitor.visit_ty( v )
}

pub fn walk_ty_fn<'a, V : Visitor<'a>>( (ar, re) : (&'a [Type], &'a Type), visitor : &mut V ) {
  visitor.visit_tys( ar );
  visitor.visit_ty( re );
}

pub fn walk_ty_generic<'a, V : Visitor<'a>>( (c, p) : (&'a Ident, &'a [Ident]), visitor : &mut V ) {
  visitor.visit_ident( c );
  p.iter().map( |prm| visitor.visit_ident( prm ) );
}

pub fn walk_ty_abstract_fn<'a, V : Visitor<'a>>( (ar, re) : (&'a [Type], &'a Type), visitor : &mut V ) {
  visitor.visit_tys( ar );
  visitor.visit_ty( re );
}

pub fn walk_ty_closure<'a, V : Visitor<'a>>( (en, ar, re) : (&'a [Type], &'a [Type], &'a Type), visitor : &mut V ) {
  visitor.visit_tys( en );
  visitor.visit_tys( ar );
  visitor.visit_ty( re );
}

pub fn walk_ty_application<'a, V : Visitor<'a>>( (ty, _) : (&'a Type, u32), visitor : &mut V ) {
  visitor.visit_ty( ty );
}


pub fn walk_class<'a, V : Visitor<'a>>( v : &'a Class, visitor : &mut V ) {
  visitor.visit_ident( &v.name );
  visitor.visit_tys( &v.params );
}

pub fn walk_expr<'a, V : Visitor<'a>>( v : &'a Expression, visitor : &mut V ) {
  visitor.visit_expr_kind( &v.kind );
  visitor.visit_ty( &v.ty );
}

pub fn walk_exprs<'a, V : Visitor<'a>>( v : &'a [Expression], visitor : &mut V ) {
  v.iter().map( |elm| visitor.visit_expr( elm ) );
}

pub fn walk_expr_kind<'a, V : Visitor<'a>>( v : &'a ExpressionKind, visitor : &mut V ) {
  match v {
    &EK::Let( ref fs, ref bdy ) => visitor.visit_expr_kind_let( (fs, bdy) ),
    &EK::UnresolvedNamed( ref idt ) => visitor.visit_expr_kind_unresolved_name( idt ),
    &EK::Apply( ref exprs ) => visitor.visit_expr_kind_apply( exprs ),
    &EK::If( ref cnd, ref thn, ref els ) => visitor.visit_expr_kind_if( (cnd, thn, els) ),
    &EK::Literal( ref lit ) => visitor.visit_expr_kind_literal( lit ),
    &EK::Arg( ref idt ) => visitor.visit_expr_kind_arg( idt ),
    &EK::Named( ref nam ) => visitor.visit_expr_kind_named( nam ),
    &EK::BuiltinFn( ref bif ) => visitor.visit_expr_kind_builtin_fn( bif ),
    &EK::FnCall( ref callee, ref args ) => visitor.visit_expr_kind_fn_call( (callee, args) ),
    &EK::Invalid => visitor.visit_expr_kind_invalid()
  }
}

pub fn walk_expr_kind_let<'a, V : Visitor<'a>>( (fs, ex) : (&'a [Function], &'a Expression), visitor : &mut V ) {
  fs.iter().map( |f| visitor.visit_fn( f ) );
  visitor.visit_expr( ex );
}

pub fn walk_expr_kind_unresolved_name<'a, V : Visitor<'a>>( v : &'a Ident, visitor : &mut V ) {
  visitor.visit_ident( v )
}

pub fn walk_expr_kind_apply<'a, V : Visitor<'a>>( v : &'a [Expression], visitor : &mut V ) {
  visitor.visit_exprs( v )
}

pub fn walk_expr_kind_if<'a, V : Visitor<'a>>( (c, t, e) : (&'a Expression, &'a Expression, &'a Expression), visitor : &mut V ) {
  visitor.visit_expr( c );
  visitor.visit_expr( t );
  visitor.visit_expr( e );
}

pub fn walk_expr_kind_literal<'a, V : Visitor<'a>>( v : &'a Literal, visitor : &mut V ) {
  visitor.visit_literal( v );
}

pub fn walk_expr_kind_arg<'a, V : Visitor<'a>>( v : &'a Ident, visitor : &mut V ) {
  visitor.visit_ident( v );
}

pub fn walk_expr_kind_named<'a, V : Visitor<'a>>( v : &'a Name, visitor : &mut V ) {
  visitor.visit_name( v );
}

pub fn walk_expr_kind_fn_call<'a, V : Visitor<'a>>( (cl, ar) : (&'a Expression, &'a [Expression]), visitor : &mut V ) {
  visitor.visit_expr( cl );
  visitor.visit_exprs( ar );
}

pub fn walk_expr_kind_builtin_fn<'a, V : Visitor<'a>>( v : &'a BuiltinFn, visitor : &mut V ) {
  visitor.visit_builtin_fn( v );
}
