use filemap::{CharLoc, Loc};
use tokenizer;
use tokenizer::{Token, TokenKind};

type TLiteral = tokenizer::Literal;

#[derive(Show, Clone, PartialEq)]
pub enum BuiltinType {
  Int
}

#[derive(Show, Clone, PartialEq)]
pub enum Type {
  NamedType( Ident ),
  BuiltinType( BuiltinType ),
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
pub struct Function {
  pub name : Ident,
  pub ty   : Type,
  pub arg_names : Vec<Ident>,
  pub body : Option<Expression>,
}

impl Function {
  pub fn new( name : Ident, arg_names : Vec<Ident>, ty : Type
            , body : Option<Expression> ) -> Function {
    
    Function { name: name
             , ty : ty
             , arg_names: arg_names
             , body: body }
  }
}

impl Loc for Function {
  fn loc( &self ) -> CharLoc {
    self.name.loc()
  }
}

#[derive(Show)]
pub enum Expression {
  Let( Vec<Function>, Box<Expression> ),
  If( Box<Expression>, Box<Expression>, Box<Expression> ),
  Literal( Literal ),
  Named( Ident ),
  Apply( Vec<Expression> )
}

#[derive(Show)]
pub struct Literal {
  pub lit : TLiteral,
  pub loc : CharLoc
}

impl Literal {
  pub fn from_token( tk : &Token ) -> Literal {
    if !tk.is_literal() {
      panic!( "Tried to create an literal from a non-literal token!" )
    }

    Literal{ lit: tk.get_literal(), loc: tk.loc() }
  }

}

impl Loc for Literal {
  fn loc( &self ) -> CharLoc {
    self.loc
  }
}

#[derive(Show, Clone)]
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

impl PartialEq<Ident> for Ident {
  fn eq( &self, other : &Ident ) -> bool {
    self.text == other.text
  }
}
