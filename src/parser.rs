 
use std::slice::Iter;

use filemap::{CharLoc, CharOffset};
use streamreader::{StreamReader, Checkpoint};
use tokenizer::{Token, TokenKind};
use ast::{ModuleItem, FunctionBody, Ident, Type, FunctionDecl
         , ToModuleItem, BuiltinType, Expression, Literal};

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
      &ParserError::SyntaxError( tk, _, _ )
        => ParserError::SyntaxError( tk, whre, wht ),
      o => panic!( "Can't overhaul context of the variant: {:?}", o )
    }
  }
}

// For brevity
type PResult<T> = Result<T, ParserError>;

impl<'a> Parser<'a> {
  fn new( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> Parser<'a> {
    Parser  { tokens: tokens
            , items: tokens.iter()
                       // Garbage initial token
            , current: None
            , checkpoints: Vec::new() }
  }

  pub fn parse( name : &'a str, src : &'a str, tokens : &'a[Token] )
     -> PResult<Vec<ModuleItem>> {
    let mut parser = Parser::new( name, src, tokens );
    parser.start()
  }

  fn start( &mut self ) -> PResult<Vec<ModuleItem>> {
    // Load up the fist token from the stream
    self.next();

    let mut mod_items = Vec::new();

    while !self.reached_end() {
      self.reset_checkpoints();
      mod_items.push( try!( self.module_item() ) );
    }
    
    Ok( mod_items )
  }

  fn module_item( &mut self ) -> PResult<ModuleItem> {
    self.push_checkpoint();
    
    self.function() // Or a typedef at some point
  }

  fn function( &mut self ) -> PResult<ModuleItem> {
    Ok( match try!( self.function_decl() ) {
      Some( v ) => v.to_module_item(),
      _ => try!( self.function_body() ).to_module_item()
    } )
  }

  fn function_body( &mut self ) -> PResult<FunctionBody> {
    let first = try!( self.try_current() );
    let name = if first.is_ident() {
      first
    } else {
      return Err( self.syntax_error( "function body", "an ident" ) )
    };

    let mut args = Vec::new();

    let mut tk;
    loop { 
      self.next();

      tk = try!( self.try_current() );

      if tk.is_ident() {
        args.push( Ident::from_token( tk ) );
      } else {
        break
      }
    }

    if !tk.is_symbol( "=" ) {
      return Err( ParserError::SyntaxError( tk.clone(), "function body", "'='" ) )
    }

    self.next();

    let body = try!( self.expression() );

    Ok( FunctionBody::new( Ident::from_token( name ), args ) )
  }

  fn function_decl( &mut self ) -> PResult<Option<FunctionDecl>> {
    let first = try!( self.try_current() );
    let name = if first.is_ident() {
      first
    } else {
      return Ok( None )
    };

    let tk = self.next();

    if !tk.is_symbol( ":" ) {
      self.peek_checkpoint();
      return Ok( None )
    }

    self.next();

    let ty = try!( self.any_type() );

    let decl = FunctionDecl::new( Ident::from_token( name ), ty );
    
    Ok( Some( decl ) )
  }

  fn any_type( &mut self ) -> PResult<Type> {
    if self.get_current().is_symbol( "(" ) {
      self.next();
      
      let ty = try!( self.fn_type() );
      if !self.get_current().is_symbol( ")" ) {
        return Err( self.syntax_error( "type", "')'" ) )
      }
      self.next();
      
      Ok( ty )
    } else {
      self.fn_type()
    }
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
      args.push( try!( self.any_type() ) );
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
    self.builtin_type()
  }

  fn builtin_type( &mut self ) -> PResult<Type> {
    let first = self.get_current();
    if !first.is_ident() {
      return Err( self.syntax_error( "type", "a builtin type" ) )
    }
    let ret = match &first.get_text()[] {
      "int" => Ok( Type::Builtin( BuiltinType::Int ) ),
      _ => Err( self.syntax_error( "type", "a builtin type" ) )
    };
    self.next();

    ret
  }

  fn expression( &mut self ) -> PResult<Expression> {
    self.low_expr()
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
        Expression::Named( Ident::from_token( self.get_current() ) )

      } else if self.get_current().is_literal() {
        Expression::Literal( Literal::from_token( self.get_current() ) )

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

    let then = try!( self.expression()
                         .map_err( |og|
                            og.overhaul_context( "if then"
                                               , "valid expression" ) ) );

    if !self.get_current().is_keyword( "else" ) {
      return Err( self.syntax_error( "if expression", "'else'" ) )
    }

    let els = try!( self.expression()
                        .map_err( |og|
                          og.overhaul_context( "if else"
                                             , "valid expression" ) ) );
    
    let ret = Expression::If( Box::new( cond )
                            , Box::new( then )
                            , Box::new( els  ) );

    Ok( Some( ret ) )
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
