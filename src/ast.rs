use std::mem::replace;

use filemap::{CharLoc, Loc};
use tokenizer;
use tokenizer::{Token, TokenKind};
use builtin::{BuiltinType, BuiltinFn};

type TLiteral = tokenizer::Literal;

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
  NamedType( Ident ),
  Unit,
  Tuple( Vec<Type> ),
  List( Box<Type> ),
  Fn( Vec<Type>, Box<Type> ),
  // Only appears after types have been resolved
  BuiltinType( BuiltinType ),
  // An application type, a type representing a partially
  // applied function. Containing the function's type and
  // how many of the arguments have been applied so far.
  // It's comparable in type with a function who's only
  // taking the arguments the partial application is missing.
  // Application( Int, Int -> Bool 1 ) =~= Int -> Bool
  Application( Box<Type>, u32 ),
  // Should never appear in a finished AST, it's only a placeholder
  // for expression who haven't gotten their type resolved yet
  Untyped
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

#[derive(Debug)]
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

#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub struct Name {
  pub name : Vec<String>,
  pub loc  : Option<CharLoc>
}

impl Name {
  // Should only be used to create toplevel names with
  // child creating functions since it's CharLoc is not accurate
  pub fn root() -> Name {
    Name { name: Vec::new(), loc: None }
  }

  pub fn from_ident( idt : &Ident, mut mscope : Option<Vec<String>> ) -> Name {
    let mut scope = mscope.unwrap_or( Vec::new() );
    scope.push( idt.text.clone() );
    Name { name: scope, loc: Some( idt.loc ) }
  }

  pub fn to_string( &self ) -> String {
    let mut r = String::new();
    let mut first = true;
    
    for frg in self.name.iter() {
      if !first {
        r.push_str( "::" );
      }
      r.push_str( &frg[] );

      first = false;
    }
    r
  }

  pub fn push( &mut self, name : String ) {
    self.name.push( name );
  }

  pub fn pop( &mut self ) {
    assert!( self.name.pop().is_some() );
  }

  pub fn change( &mut self, name : String ) {
    assert!( self.name.len() > 0 );
    *self.name.last_mut().unwrap() = name;
  }

  pub fn scope( &mut self, scpe : &mut Vec<String> ) {
    let mut end = replace( &mut self.name, scpe.clone() );
    self.name.append( &mut end );
  }

  pub fn ident_child( &self, name : &Ident ) -> Name {
    Name::from_ident( name, Some( self.name.clone() ) )
  }

  pub fn matches( &self, scope : &Name, name : &str ) -> bool {
    // Check if they origin scope and the name matches the Name
    // without having to allocate a whole new Name to check against
    self.name.init() == scope.name && self.name.last()
                                               .map( |v| &v[] == name )
                                               .unwrap_or( false )
  }

  pub fn is_toplevel( &self ) -> bool {
    self.name.len() <= 1
  }

  pub fn no_loc( &mut self ) {
    self.loc = None;
  }

  pub fn same( &self, other : &Name ) -> bool {
    self.name == other.name
  }

}

#[derive(Debug)]
pub enum ExpressionKind {
  Let( Vec<Function>, Box<Expression> ),
  UnresolvedNamed( Ident ),
  Apply( Vec<Expression> ),
  // The above will be phased out of the tree
  If( Box<Expression>, Box<Expression>, Box<Expression> ),
  Literal( Literal ),
  // Can't be created from the parser:
  Arg( Ident ),
  Named( Name ),
  BuiltinFn( BuiltinFn ),
  // The call nodes are transformed from Apply
  BuiltinCall( BuiltinFn, Vec<Expression> ),
  FnCall( Box<Expression>, Vec<Expression> ),
  // PartialApplication
  // PartialFinsiher
  // Should never appear in a fully built expression
  Invalid
}

#[derive(Debug)]
pub struct Expression {
  pub kind : ExpressionKind,
  pub ty   : Type
}

impl Expression {
  pub fn new( k : ExpressionKind, t : Type ) -> Expression {
    Expression { kind: k, ty: t }
  }
}

// Untyped expressipn
pub fn uexpr( ek : ExpressionKind ) -> Expression {
  Expression { kind: ek, ty: Type::Untyped }
}

#[derive(Debug)]
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

  pub fn get_type( &self ) -> Type {
    match &self.lit {
      &tokenizer::Literal::Integer( .. ) => 
        Type::BuiltinType( BuiltinType::Int ),
      &tokenizer::Literal::Boolean( .. ) => 
        Type::BuiltinType( BuiltinType::Bool )
    }
  }

}

impl Loc for Literal {
  fn loc( &self ) -> CharLoc {
    self.loc
  }
}

#[derive(Debug, Clone)]
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
