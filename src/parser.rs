 
use std::slice::Iter;

use filemap::{CharLoc, CharOffset, Loc};
use streamreader::{StreamReader, Checkpoint};
use tokenizer::{Token, TokenKind};
use tokenizer; 
use ast::{Function, Ident, Type, Expression, ExpressionKind, Literal, uexpr
         , Class, TypeDefinition, Item};

// TODO: Fix identation

// ExpressionKind::* => EK::*
#[allow(non_snake_case)]
mod EK {
  pub use ast::ExpressionKind::*;
}

pub struct Parser<'a> {
  items   : Iter<'a, Token>,
  current : Option<&'a Token>,
  checkpoints : Vec<(&'a Token, Iter<'a, Token>)>
}

#[derive(Debug)]
pub enum ParserError {
  ReachedEof,
  SyntaxError( Token, &'static str, &'static str )
}

impl ParserError {
  fn overhaul_context( &self, whre : &'static str, wht : &'static str )
     -> ParserError {
    match self {
      &ParserError::SyntaxError( ref tk, _, _ )
        => ParserError::SyntaxError( tk.clone(), whre, wht ),
      o => panic!( "Can't overhaul context of the variant: {:?}", o )
    }
  }
}

const OPERATOR_COUNT : usize = 14;
static OPERATORS : [(&'static str, u32); OPERATOR_COUNT] =
       [ ("<|", 0), ("|>", 0) // Lowest precedence
       , ("=", 1)
       , ("==", 2), ("!=", 2)
       , ("<", 3), (">", 3), (">=", 3), ("<=", 3)
       , ("+", 4), ("-", 4)
       , ("*", 5), ("/", 5)
       , (".", 6) ]; // Highest precedence

// For brevity
pub type PResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
  fn new( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> Parser<'a> {
    Parser { items: tokens.iter()
                      // Garbage initial token
           , current: None
           , checkpoints: Vec::new() }
  }

  pub fn parse( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> PResult<Vec<Item>> {
    let mut parser = Parser::new( name, src, tokens );
    parser.start()
  }

  fn start( &mut self ) -> PResult<Vec<Item>> {
    // Load up the fist token from the stream
    self.next();

    let mut items = Vec::new();

    while !self.reached_end() {
      self.reset_checkpoints();
      items.push( try!( self.item() ) );
    }
    
    Ok( items )
  }

  fn item( &mut self ) -> PResult<Item> {
    if let Some( fun ) = try!( self.fn_def() ) {
      Ok( Item::Fn( fun ) )
    } else if let Some( ty ) = try!( self.type_def() ) {
      // This may fail, since anything else is invalid syntax
      Ok( Item::Type( ty ) )
    } else if let Some( dt ) = try!( self.data_def() ) {
      Ok( Item::Type( dt ) )
    } else {
      return Err( self.syntax_error( "item", "function definition, type alias\
 or data type definition" ) )
    }
  }

  fn fn_def( &mut self ) -> PResult<Option<Function>> {

    if !self.get_current().is_keyword( "def" ) {
      // Used to be a syntax error:
      // return Err( self.syntax_error( "function", "'def'" ) )
      return Ok( None )
    }

    self.next();

    Ok( Some( try!( self.function() ) ) )
  }

  fn function( &mut self ) -> PResult<Function> {

    let fn_name = if self.get_current().is_ident() {
      Ident::from_token( self.get_current() )
    } else {
      return Err( self.syntax_error( "function", "function name" ) )
    };

    self.next();

    // Parse the arguments
    let mut arg_names = Vec::new();

    while self.get_current().is_ident() {
      let arg_name = Ident::from_token( self.get_current() );
      
      arg_names.push( arg_name );
      
      self.next();
    }

    // Parse the return type
    if !self.get_current().is_symbol( ":" ) {
      return Err( self.syntax_error( "function header", ":" ) )
    }

    self.next();

    let ty = try!( self.any_type() );

    let constraints = try!( self.where_clause() );

    // Parse the body
    let body = if self.get_current().is_symbol( "=" ) {
      self.next();

      Some( try!( self.expression() ) )
    } else {
      None
    };
    Ok( Function::new( fn_name, arg_names, ty, body, constraints ) )
  }

  fn any_type( &mut self ) -> PResult<Type> {
    self.fn_type()
  }

  fn fn_type( &mut self ) -> PResult<Type> {
    let start_ty = try!( self._type() );
    
    let start = self.get_current();
    
    let mut arg_types = Vec::new();

    if !( start.is_symbol( "->" ) ) {
      self.push_checkpoint();

      let ty = match self._type() {
        Ok( t ) => t,
        Err( .. ) => {
          self.pop_checkpoint();
          return Ok( start_ty )
        }
      };

      arg_types.push( start_ty );
      arg_types.push( ty );
    } else {
      arg_types.push( start_ty );
    }

    while !self.get_current().is_symbol( "->" ) {
      arg_types.push( try!( self._type() ) );
    }

    self.next();

    let ret_ty = try!( self.any_type() );

    Ok( Type::Fn( arg_types, Box::new( ret_ty ) ) )
  }

  fn _type( &mut self ) -> PResult<Type> {
    if let Some( tup_ty ) = try!( self.tuple_type() ) {
      Ok( tup_ty )

    } else if let Some( list_ty ) = try!( self.list_type() ) {
        Ok( list_ty )

    } else {
      self.named_type()
    }
  }

  fn tuple_type( &mut self ) -> PResult<Option<Type>> {
    if !self.get_current().is_symbol( "(" ) {
      return Ok( None )
    }

    self.next();

    if self.get_current().is_symbol( ")" ) {
      self.next();

      return Ok( Some( Type::Unit ) )
    }

    let first_ty = try!( self.any_type() );
    
    if !self.get_current().is_symbol( "," ) {
      if !self.get_current().is_symbol( ")" ) {
        return Err( self.syntax_error( "type", "')'" ) )
      }

      self.next();

      return Ok( Some( first_ty ) )
    }

    self.next();

    let mut types = vec![ first_ty ];
    if self.get_current().is_symbol( ")" ) {
      self.next();

      return Ok( Some( Type::Tuple( types ) ) )
    }

    loop {
      types.push( try!( self.any_type() ) );

      if !self.get_current().is_symbol( "," ) {
        break;
      }

      self.next();
    }

    if !self.get_current().is_symbol( ")" ) {
      return Err( self.syntax_error( "type", "')'" ) )
    }

    self.next();
      
    Ok( Some( Type::Tuple( types ) ) )
  }

  fn list_type( &mut self ) -> PResult<Option<Type>> {
    if !self.get_current().is_symbol( "[" ) {
      return Ok( None )
    }

    self.next();

    let inner = try!( self.any_type() );

    if !self.get_current().is_symbol( "]" ) {
      return Err( self.syntax_error( "list type", "']'" ) )
    }

    self.next();

    Ok( Some( Type::List( Box::new( inner ) ) ) )
    
  }

  fn named_type( &mut self ) -> PResult<Type> {
    let first = self.get_current();

    if first.is_type_name() {
      self.next();

      Ok( Type::NamedType( Ident::from_token( &first.as_ident() ) ) )
    } else if first.is_ident() {
      self.next();

      Ok( Type::Generic( Ident::from_token( first ), Vec::new() ) )
    } else {
      Err( self.syntax_error( "type"
                            , "a type name or generic type parameter" ) )
    }
  }

  fn where_clause( &mut self ) -> PResult<Vec<Class>> {
    if !self.get_current().is_keyword( "where" ) {
      return Ok( Vec::new() )
    }

    self.next();

    let mut constraints = Vec::new();

    loop {
      constraints.push( try!( self.class() ) );
      if !self.get_current().is_symbol( "," ) {
        break
      }
      self.next();
    }

    Ok( constraints )
  }

  fn class( &mut self ) -> PResult<Class> {
    let first = self.get_current();

    if !first.is_type_name() {
      return Err( self.syntax_error( "class", "class name" ) )
    }

    let cname = Ident::from_token( &first.as_ident() );
    let mut cargs = Vec::new();

    self.next();

    loop {
      self.push_checkpoint();

      match self.any_type() {
        Ok( ty ) => cargs.push( ty ),
        Err( _ ) => {
          self.pop_checkpoint();
          break
        }
      }
    }

    Ok( Class{ name: cname, params: cargs } )
  }

  fn expression( &mut self ) -> PResult<Expression> {
    let first = try!( self.op_expr( 0 ) );

    let mut rest = vec![ first ];
    loop {
      self.push_checkpoint();

      match self.op_expr( 0 ) {
        Ok( o ) => rest.push( o ),
        Err( _ ) => {
          self.pop_checkpoint();
          break;
        }
      }
    }

    if rest.len() == 1 {
      Ok( rest.pop().unwrap() )
    } else {
      Ok( uexpr( EK::Apply( rest ) ) )
    }
  }

  fn op_expr( &mut self, op_precedence : u32 ) -> PResult<Expression> {
    // Get all the OPERATORS that match the current precedence level.
    let mut ops = OPERATORS.iter()
                           .filter( |&&(_, prec)| prec == op_precedence );
    
    let mut lhs = try!( self.lower_op_expr( op_precedence ) );
    
    for &(op, _) in ops {
      while self.get_current().is_symbol( op ) {
        let op_tk = uexpr( EK::UnresolvedNamed(
                      Ident::from_token( &self.get_current()
                                              .as_ident() ) ) );
        
        self.next();

        let rhs = try!( self.lower_op_expr( op_precedence ) );
        // lhs <op> rhs becomes <op> lhs rhs 
        lhs = uexpr( EK::Apply( vec![ op_tk, lhs, rhs ] ) );
      }
    }

    // In case we don't match an operator just return the fallthrough value
    Ok( lhs )
  }

  fn lower_op_expr( &mut self, op_precedence : u32 ) -> PResult<Expression> {
    if op_precedence == OPERATOR_COUNT as u32 - 1 {
      self.low_expr()
    } else {
      self.op_expr( op_precedence + 1 )
    }
  }

  fn low_expr( &mut self ) -> PResult<Expression> {
    Ok( if self.get_current().is_symbol( "(" ) {

      let unit_loc = self.get_current().loc();
      
      self.next();
      
      if self.get_current().is_symbol( ")" ) {
        self.next();
        return Ok( Expression::new(
            EK::Literal( Literal::new( tokenizer::Literal::Unit
                                     , unit_loc ) )
            , Type::Unit ) )
      }

      let expr = try!( self.expression() );
      if !self.get_current().is_symbol( ")" ) {
        return Err( self.syntax_error( "expression", "')'" ) )
      }

      self.next();

      expr
    } else {
      if let Some( if_expr ) = try!( self.if_expr() ) {
        if_expr

      } else if let Some( let_expr ) = try!( self.let_expr() ) {
        let_expr

      } else if self.get_current().is_ident() {
        let r = uexpr( EK::UnresolvedNamed(
                  Ident::from_token( self.get_current() ) ) );
        self.next();
        r

      } else if self.get_current().is_literal() {
        let lit = Literal::from_token( self.get_current() );
        let ty = lit.get_type();

        let r = Expression::new( EK::Literal( lit ), ty );
        self.next();
        r

      } else {
        return Err( self.syntax_error( "expression", "a valid expression" ) )
      }
    } )
  }

  fn if_expr( &mut self ) -> PResult<Option<Expression>> {
    if !self.get_current().is_keyword( "if" ) {
      return Ok( None )
    }

    self.next();

    let cond = try!( self.expression()
                         .map_err( |og|
                            og.overhaul_context( "if condition"
                                               , "valid condition" ) ) );

    if !self.get_current().is_keyword( "then" ) {
      return Err( self.syntax_error( "if expression", "'then'" ) )
    }

    self.next();

    let then = try!( self.expression()
                         .map_err( |og|
                            og.overhaul_context( "if then"
                                               , "valid expression" ) ) );

    if !self.get_current().is_keyword( "else" ) {
      return Err( self.syntax_error( "if expression", "'else'" ) )
    }

    self.next();

    let els = try!( self.expression()
                        .map_err( |og|
                          og.overhaul_context( "if else"
                                             , "valid expression" ) ) );
    
    let ret = uexpr( EK::If( Box::new( cond )
                            , Box::new( then )
                            , Box::new( els  ) ) );

    Ok( Some( ret ) )
  }

  fn let_expr( &mut self ) -> PResult<Option<Expression>> {
    if !self.get_current().is_keyword( "let" ) {
      return Ok( None )
    }

    self.next();

    let mut let_items = Vec::new();

    loop {
      let_items.push( try!( self.function() ) );
      if !self.get_current().is_symbol( "," ) {
        break
      }
      self.next();
    }

    if !self.get_current().is_keyword( "in" ) {
      return Err( self.syntax_error( "let expression", "'in'" ) )
    }
    self.next();

    let expr = try!( self.expression() );

    Ok( Some( uexpr( EK::Let( let_items, Box::new( expr ) ) ) ) )
  }

  fn type_def( &mut self ) -> PResult<Option<TypeDefinition>> {

    if !self.get_current().is_keyword( "type" ) {
      // Used to return an error:
      // return Err( self.syntax_error( "type definition", "'type'" ) )
      return Ok( None )
    }

    self.next();

    if !self.get_current().is_type_name() {
      return Err( self.syntax_error( "type definition", "type name" ) )
    }

    let name = Ident::from_token( &self.get_current().as_ident() );

    self.next();

    if !self.get_current().is_symbol( "=" ) {
      return Err( self.syntax_error( "type definition", "'='" ) ) 
    }

    self.next();

    let base = try!( self.any_type() );

    Ok( Some( TypeDefinition::Alias( name, base ) ) )
  }

  fn data_def( &mut self ) -> PResult<Option<TypeDefinition>> {

    if !self.get_current().is_keyword( "data" ) {
      return Ok( None )
    }

    self.next();

    if !self.get_current().is_type_name() {
      return Err( self.syntax_error( "data type definition"
                                   , "data type name" ) )
    }

    let name = Ident::from_token( &self.get_current().as_ident() );
    let mut body : Type;

    self.next();

    if !self.get_current().is_symbol( "=" ) {
      return Err( self.syntax_error( "data type definition", "'='" ) )
    }

    self.next();

    if !self.get_current().is_ident() {
      body = try!( self.any_type() );
      return Ok( Some( TypeDefinition::Data( Type::Unique( name
                                                         , Box::new( body ) ) ) ) )
    }

    let mut fields = Vec::new();

    loop {
      // Parse the field name
      if !self.get_current().is_ident() {
        return Err( self.syntax_error( "data type definition fields", "field name" ) ) 
      }

      let field_name = Ident::from_token( self.get_current() );

      self.next();

      if !self.get_current().is_symbol( ":" ) {
        return Err( self.syntax_error( "data type definition fields", "':'" ) )
      }

      self.next();

      // Parse the field type
      let field_type = try!( self.any_type() );

      fields.push( (field_name, field_type) );

      // Continue if a colon is present
      if !self.get_current().is_symbol( "," ) {
        break;
      }

      self.next();
    }

    body = Type::Structure( fields );

    Ok( Some( TypeDefinition::Data( Type::Unique( name, Box::new( body ) ) ) ) )
  }

  fn syntax_error( &mut self, whre : &'static str, wht : &'static str )
     -> ParserError {
    ParserError::SyntaxError( self.get_current().clone(), whre, wht )
  }

}

impl<'a> StreamReader<&'a Token, &'a Token, ParserError> for Parser<'a> {
  
  fn next( &mut self ) -> &'a Token {
    self.current = Some( match self.items.next() {
      Some( tk ) => tk,
      // This should always be a EOF token since that's the last token
      None => self.get_current() 
    } );
    self.current.unwrap()
  }

  fn set_current( &mut self, v : &'a Token ) {
    self.current = Some( v );
  }

  fn get_current( &self ) -> &'a Token {
    self.current.unwrap()
  }

  fn reached_end( &self ) -> bool {
    self.get_current().is_eof()
  }

  fn try_current( &self ) -> PResult<&'a Token> {
    if self.get_current().is_eof() {
      Err( ParserError::ReachedEof )
    } else {
      Ok( self.get_current() )
    }
  }
}

impl<'a> Checkpoint for Parser<'a> {
  fn push_checkpoint( &mut self ) {
    let cur = self.get_current();
    self.checkpoints.push( (cur, self.items.clone()) );
  }

  fn pop_checkpoint( &mut self ) {
    match self.checkpoints.pop() {
      None => panic!( "Tried to pop a checkpoint from an empty checkpoint stack" ),
      Some( (c, v) ) => {
        self.set_current( c );
        self.items = v;
      }
    }
  }

  fn reset_checkpoints( &mut self ) {
    self.checkpoints.clear()
  }
}
