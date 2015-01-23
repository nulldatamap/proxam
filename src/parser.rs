 
use std::slice::Iter;

use filemap::{CharLoc, CharOffset};
use streamreader::{StreamReader, Checkpoint};
use tokenizer::{Token, TokenKind};
use ast::{Function, Ident, Type, BuiltinType, Expression, Literal};

pub struct Parser<'a> {
  items   : Iter<'a, Token>,
  tokens  : &'a [Token],
  current : Option<&'a Token>,
  checkpoints : Vec<(&'a Token, Iter<'a, Token>)>
}

#[derive(Show)]
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

const operator_count : usize = 14;
static operators : [(&'static str, u32); operator_count] =
       [ ("<|", 0), ("|>", 0) // Lowest precedence
       , ("=", 1)
       , ("==", 2), ("/=", 2)
       , ("<", 3), (">", 3), (">=", 3), ("<=", 3)
       , ("+", 4), ("-", 4)
       , ("*", 5), ("/", 5)
       , (".", 6) ]; // Highest precedence

// For brevity
type PResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
  fn new( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> Parser<'a> {
    Parser { tokens: tokens
           , items: tokens.iter()
                      // Garbage initial token
           , current: None
           , checkpoints: Vec::new() }
  }

  pub fn parse( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> PResult<Vec<Function>> {
    let mut parser = Parser::new( name, src, tokens );
    parser.start()
  }

  fn start( &mut self ) -> PResult<Vec<Function>> {
    // Load up the fist token from the stream
    self.next();

    let mut fns = Vec::new();

    while !self.reached_end() {
      self.reset_checkpoints();
      fns.push( try!( self.def() ) );
    }
    
    Ok( fns )
  }

  fn def( &mut self ) -> PResult<Function> {

    if !self.get_current().is_keyword( "def" ) {
      return Err( self.syntax_error( "function", "'def'" ) )
    }

    self.next();

    self.function()
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

    // Parse the body
    let body = if self.get_current().is_symbol( "=" ) {
      self.next();

      Some( try!( self.expression() ) )
    } else {
      None
    };
    Ok( Function::new( fn_name, arg_names, ty, body ) )
  }

  fn any_type( &mut self ) -> PResult<Type> {
    self.fn_type()
  }

  fn fn_type( &mut self ) -> PResult<Type> {
    let start_ty = try!( self._type() );
    
    let start = self.get_current();

    if !( start.is_symbol( "->" ) || start.is_symbol( "," ) ) {
      return Ok( start_ty )
    }

    let mut args = vec![ start_ty ];

    while self.get_current().is_symbol( "," ) {
      self.next();
      args.push( try!( self._type() ) );
    }

    if !self.get_current().is_symbol( "->" ) {
      return Err( self.syntax_error( "function type", "'->'" ) )
    }

    self.next();

    let ret = try!( self.any_type() );

    let ty = Type::Fn( args, Box::new( ret ) );

    Ok( ty )
  }

  fn _type( &mut self ) -> PResult<Type> {
    if self.get_current().is_symbol( "(" ) {
      self.next();
      
      let ty = try!( self.any_type() );
      if !self.get_current().is_symbol( ")" ) {
        return Err( self.syntax_error( "type", "')'" ) )
      }
      self.next();
      
      Ok( ty )
    } else {
      self.named_type()
    }
  }

  fn named_type( &mut self ) -> PResult<Type> {
    let first = self.get_current();

    if !first.is_type_name() {
      return Err( self.syntax_error( "type", "a builtin type" ) )
    }

    self.next();

    Ok( Type::NamedType( Ident::from_token( &first.as_ident() ) ) )
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
      Ok( Expression::Apply( rest ) )
    }
  }

  fn op_expr( &mut self, op_precedence : u32 ) -> PResult<Expression> {
    // Get all the operators that match the current precedence level.
    let mut ops = operators.iter()
                           .filter( |&&(_, prec)| prec == op_precedence );
    
    let lhs = try!( self.lower_op_expr( op_precedence ) );
    
    for &(op, _) in ops {
      if self.get_current().is_symbol( op ) {
        let op_tk = Expression::Named(
                      Ident::from_token( &self.get_current()
                                              .as_ident() ) );
        
        self.next();

        let rhs = try!( self.lower_op_expr( op_precedence ) );
        // lhs <op> rhs becomes <op> lhs rhs 
        let expr = Expression::Apply( vec![ op_tk, lhs, rhs ] );
        return Ok( expr )
      }
    }

    // In case we don't match an operator just return the fallthrough value
    Ok( lhs )
  }

  fn lower_op_expr( &mut self, op_precedence : u32 ) -> PResult<Expression> {
    if op_precedence == operator_count as u32 - 1 {
      self.low_expr()
    } else {
      self.op_expr( op_precedence + 1 )
    }
  }

  fn low_expr( &mut self ) -> PResult<Expression> {
    Ok( if self.get_current().is_symbol( "(" ) {

      self.next();
      
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
        let r = Expression::Named( Ident::from_token( self.get_current() ) );
        self.next();
        r

      } else if self.get_current().is_literal() {
        let r = Expression::Literal( Literal::from_token( self.get_current() ) );
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
    
    let ret = Expression::If( Box::new( cond )
                            , Box::new( then )
                            , Box::new( els  ) );

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
      print!(", ");
      self.next();
    }

    if !self.get_current().is_keyword( "in" ) {
      return Err( self.syntax_error( "let expression", "'in'" ) );
    }
    self.next();

    let expr = try!( self.expression() );

    Ok( Some( Expression::Let( let_items, Box::new( expr ) ) ) )
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
    self.checkpoints.push( (cur, self.items) );
  }

  fn peek_checkpoint( &mut self ) {
    match self.checkpoints.last() {
      None => panic!( "Tried to peek a checkpoint from an empty checkpoint stack" ),
      Some( &(c, v) ) => {
        self.set_current( c );
        self.items = v;
      }
    }
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
