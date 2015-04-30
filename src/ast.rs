use std::mem::replace;
use std::fmt;

use filemap::{CharLoc, Loc};
use visitor::Visitor;
use tokenizer;
use tokenizer::{Token, TokenKind};
use builtin::{BuiltinType, BuiltinFn};

pub type TLiteral = tokenizer::Literal;

#[derive(Clone, PartialEq)]
pub enum Type {
  NamedType( Ident ),
  Unit,
  Unique( Ident, Box<Type> ),
  Tuple( Vec<Type> ),
  Structure( Vec<(Ident, Type)> ),
  List( Box<Type> ),
  Fn( Vec<Type>, Box<Type> ),
  // A generic type parameter with possible constraints
  Generic( Ident, Vec<Ident> ),
  // A generic function type which bare functions, partially applied functions
  // and closures can be coerced to 
  AbstractFn( Vec<Type>, Box<Type> ),
  // A closure type which both has it's environment values and arguments
  Closure( Vec<Type>, Vec<Type>, Box<Type> ),
  // Only appears after types have been resolved
  BuiltinType( BuiltinType ),
  // An application type, a type representing a partially
  // applied function. Containing the function's type and
  // how many of the arguments have been applied so far.
  // It's comparable in type with a function who's only
  // taking the arguments the partial application is missing.
  // Application( Int, Int -> Bool 1 ) =~= Int -> Bool
  Application( Box<Type>, u32 ),
  // Should never appear in a finished AST, it's only a place-holder
  // for expression who haven't gotten their type resolved yet
  Untyped
}

impl fmt::Debug for Type {
  fn fmt( &self, f : &mut fmt::Formatter ) -> Result<(), fmt::Error> {
    match self {
      &Type::NamedType( ref n ) => write!( f, "<{}>", n.text ),
      &Type::Unit => write!( f, "()" ),
      &Type::Unique( ref id, ref ty ) => write!( f, "{}:{:?}", id.text, ty ),
      &Type::Structure( ref pairs ) => {
        let _ = write!( f, "{{" );
        for &(ref id, ref ty) in pairs {
          let _ = write!( f, "{} : {:?},", id.text, ty );
        }
        write!( f, "}}" )
      },
      &Type::Tuple( ref els ) => {
        let _ = write!( f, "(" );
        for el in els { 
          let _ = write!( f, "{:?}, ", el );
        }
        write!( f, ")" )
      },
      &Type::List( ref t ) => write!( f, "[{:?}]", t ),
      &Type::Fn( ref args, ref r ) => {
        for el in args { 
          let _ = write!( f, "{:?}, ", el );
        }
        write!( f, "-> {:?}", r )
      },
      &Type::BuiltinType( ref bit ) => write!( f, "{:?}", bit ),
      _ => write!( f, "<WUT_TYPE>" )
    }
  }
}

impl Type {
  pub fn argument_count( &self ) -> usize {
    match self {
      &Type::Fn( ref args, _ ) => args.len(),
      _ => 0
    }
  }

  pub fn argument_type( &self, idx : usize ) -> &Type {
    match self {
      &Type::Fn( ref args, _ ) => {
        assert!( idx < args.len() );
        &args[idx]
      },
      _ => panic!( "{:?} is not a function type.", self )
    }
  }

  pub fn return_type( &self ) -> &Type {
    match self {
      &Type::Fn( _, ref ret ) => {
        &**ret
      },
      _ => panic!( "{:?} is not a function type.", self )
    }
  }

  pub fn is_call_coercible( &self, ty : &Type ) -> bool {
    match self {
      &Type::Fn( .. ) => {
        // The function must have no argumens, and the return type must be the 
        // same as the output type or coercible to the output type
        self.argument_count() == 0
        && ( self.return_type() == ty || self.return_type()
                                             .is_call_coercible( ty ) )
      },
      _ => false
    }
  }

  // Converts a NON-FUNCTION type to a function type
  pub fn to_fn( &mut self ) {
    let t = replace( self, Type::Untyped );
    *self = match t {
      Type::Fn( .. ) => t,
      _ => Type::Fn( Vec::new(), Box::new( t ) )
    };
  }

  pub fn is_fn_type( &self ) -> bool {
    match self {
      &Type::Fn( .. ) => true,
      _ => false
    }
  }

}


#[derive(Debug, Clone)]
pub struct Class {
  pub name : Ident,
  pub params : Vec<Type>
}

#[derive(Debug, Clone)]
pub struct Function {
  pub name : Ident,
  pub ty   : Type,
  pub arg_names : Vec<Ident>,
  pub body : Option<Expression>,
  pub constraints : Vec<Class>
}

impl Function {
  pub fn new( name : Ident, arg_names : Vec<Ident>, ty : Type
            , body : Option<Expression>, constraints : Vec<Class> )
      -> Function {
    
    Function { name: name
             , ty : ty
             , arg_names: arg_names
             , body: body
             , constraints: constraints }
  }

  pub fn empty() -> Function {
    Function { name: Ident::empty()
             , ty: Type::Untyped
             , arg_names: Vec::new()
             , body : None
             , constraints: Vec::new() }
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
      r.push_str( &frg );

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

  pub fn ident_child( &self, name : &Ident ) -> Name {
    Name::from_ident( name, Some( self.name.clone() ) )
  }

  pub fn no_loc( &mut self ) {
    self.loc = None;
  }

  pub fn same( &self, other : &Name ) -> bool {
    self.name == other.name
  }

}

#[derive(Debug, Clone)]
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
  FnCall( Box<Expression>, Vec<Expression> ),
  // PartialApplication
  // PartialFinsiher
  // Appears after type checking
  // Should never appear in a fully built expression
  Invalid
}

#[derive(Debug, Clone)]
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
pub enum TypeDefinition {
  Alias( Ident, Type ),
  Data( Type )
}

impl TypeDefinition {
  pub fn name( &self ) -> &Ident {
    match self {
      &TypeDefinition::Alias( ref nam, _ ) => nam,
      &TypeDefinition::Data( Type::Unique( ref nam, _ ) ) => nam,
      inv => panic!( "Can't get the name of the type: {:?}", inv )
    }
  }

  pub fn inner( self ) -> Type {
    match self {
      TypeDefinition::Alias( _, inr ) => inr,
      TypeDefinition::Data( inr ) => inr 
    }
  }

}

#[derive(Debug)]
pub enum Item {
  Fn( Function ),
  Type( TypeDefinition )
}

#[derive(Debug, Clone)]
pub struct Literal {
  pub lit : TLiteral,
  pub loc : CharLoc
}

impl Literal {
  pub fn new( lit : TLiteral, loc : CharLoc ) -> Literal {
    Literal{ lit: lit, loc: loc }
  }

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
        Type::BuiltinType( BuiltinType::Bool ),
      &tokenizer::Literal::Unit =>
        Type::Unit,
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

  pub fn empty() -> Ident {
    Ident { text: String::new(), loc: CharLoc( 0 ) }
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
