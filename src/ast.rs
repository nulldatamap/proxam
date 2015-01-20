use filemap::{CharLoc, Loc};
use tokenizer::{Token, TokenKind};


#[derive(Show, Clone)]
pub enum BuiltinType {
  Int
}

#[derive(Show, Clone)]
pub enum Type {
  Builtin( BuiltinType ),
  Unit,
  Tuple( Vec<Type> ),
  Fn( Vec<Type>, Box<Type> )
}

impl Type {
  pub fn argument_count( &self ) -> u32 {
    match self {
      &Type::Fn( ref args, _ ) => args.len() as u32,
      _ => 0
    }
  }

  // Converts a NON-FUNCTION type to a function type
  pub fn to_fn( self ) -> Type {
    match self {
      f @ Type::Fn( .. ) => return f,
      v => Type::Fn( Vec::new(), Box::new( v ) )
    }
  }
}

#[derive(Show)]
pub enum ModuleItem {
  FunctionBody( FunctionBody ),
  FunctionDecl( FunctionDecl )
}


#[derive(Show)]
pub struct FunctionDecl {
  pub name : Ident,
  pub ty   : Type
}

impl FunctionDecl {
  pub fn new( name : Ident, ty : Type ) -> FunctionDecl {
    FunctionDecl { name : name
                 , ty   : ty }
  }
}

#[derive(Show)]
pub struct FunctionBody {
  pub name : Ident,
  pub args : Vec<Ident>,
  // We also need an actual body
}

impl FunctionBody {
  pub fn new( name : Ident, args : Vec<Ident> ) -> FunctionBody {
    FunctionBody { name: name
                 , args: args }
  }
}

impl Loc for FunctionBody {
  fn loc( &self ) -> CharLoc {
    self.name.loc()
  }
}

pub trait ToModuleItem {
  fn to_module_item( self ) -> ModuleItem;
}

impl ToModuleItem for FunctionBody {
  fn to_module_item( self ) -> ModuleItem {
    ModuleItem::FunctionBody( self )
  }
}

impl ToModuleItem for FunctionDecl {
  fn to_module_item( self ) -> ModuleItem {
    ModuleItem::FunctionDecl( self )
  }
}

#[derive(Show)]
pub enum Expression {
}

#[derive(Show)]
pub struct Ident {
  pub text : String,
  pub loc  : CharLoc
}

impl Ident {
  pub fn from_token( tk : &Token ) -> Ident {
    if !tk.is_ident() {
      panic!( "Tried to create an ident from a non-ident token!" )
    }
    Ident{ text: tk.get_text(), loc: tk.loc() }
  }

}

impl Loc for Ident {
  fn loc( &self ) -> CharLoc {
    self.loc
  }
}

