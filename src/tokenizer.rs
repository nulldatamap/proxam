use std::str::{CharIndices, FromStr};

use streamreader::{StreamReader, Checkpoint};

use filemap::{CharLoc, CharOffset, Loc};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum Literal {
  Integer( i64 ),
  Boolean( bool ),
  Unit
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum TokenKind {
  Comment( String ),
  Ident( String ),
  TypeName( String ),
  Symbol( String ),
  Literal( Literal ),
  Keyword( String ),
  EOF
}

#[derive(Debug, PartialEq, Eq, Clone)]
pub struct Token {
  kind : TokenKind,
  loc  : CharLoc
}

impl Token {
  pub fn new( kind : TokenKind, loc : CharLoc ) -> Token {
    
    Token{ kind: kind // That's very kind of you <3
         , loc : loc }
  }

  pub fn get_text( &self ) -> String {
    match self.kind {
      TokenKind::Ident( ref s ) => s.clone(),
      TokenKind::Symbol( ref s ) => s.clone(),
      _ => panic!( "Tried to get text from a non-text token: {:?}", self )
    }
  }

  pub fn get_literal( &self ) -> Literal {
    match self.kind {
      TokenKind::Literal( lit ) => lit,
      _ => panic!( "Tried to get Literal from a non-literal token: {:?}", self )
    }
  }

  pub fn as_ident( &self ) -> Token {
    match self.kind {
      TokenKind::Symbol( ref s ) => Token::new( TokenKind::Ident( s.clone() )
                                              , self.loc ),
      TokenKind::TypeName( ref s ) => Token::new( TokenKind::Ident( s.clone() )
                                                , self.loc ),
      _ => panic!( "Tried to turn non-symbol token into an ident: {:?}", self )
    }
  }

  pub fn is_ident( &self ) -> bool {
    match self.kind {
      TokenKind::Ident( _ ) => true,
      _ => false
    }
  }

  pub fn is_symbol( &self, sym : &str ) -> bool {
    match self.kind {
      TokenKind::Symbol( ref s ) => &s[..] == sym,
      _ => false
    }
  }

  pub fn is_keyword( &self, kwd : &str ) -> bool {
    match self.kind {
      TokenKind::Keyword( ref s ) => &s[..] == kwd,
      _ => false
    }
  }

  pub fn is_type_name( &self ) -> bool {
    match self.kind {
      TokenKind::TypeName( _ ) => true,
      _ => false
    }
  }

  fn is_comment( &self ) -> bool {
    match self.kind {
      TokenKind::Comment( _ ) => true,
      _ => false
    }
  }

  pub fn is_literal( &self ) -> bool {
    match self.kind {
      TokenKind::Literal( _ ) => true,
      _ => false
    }
  }

  pub fn is_eof( &self ) -> bool {
    match self.kind {
      TokenKind::EOF => true,
      _ => false
    }
  }

}

impl Loc for Token {
  fn loc( &self ) -> CharLoc {
    self.loc
  }
}

// A tokenizer does not own any of the strings passed to it
// since the interaction with it should be very brief and
// a tokenizer object has no need to be stored after tokenizing
// has finished.
pub struct Tokenizer<'a> {
  src          : &'a str,
  start_loc    : CharLoc,
  chars        : CharIndices<'a>,
  current_chr  : Option<char>,
  current_loc  : Option<CharLoc>,
  checkpoints  : Vec<(Option<char>, Option<CharLoc>, CharIndices<'a>)>
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenizerError {
  // We were trying to parse a token when we ran out of characters
  UnexpectedEof,
  // We found a character that doesn't fit any of the tokenizing rules
  InvalidToken( CharLoc )
}

// For brevity
pub type TResult<T> = Result<T, TokenizerError>;

// This will chain multiple parsing functions together that
// may fail in a readable manner
macro_rules! fallthrough(
  ( $subj:ident : $token:ident, $($tokens:ident),* ) => (
    try!( $subj.$token()
      $(
        .and_then( |v| if v.is_none() { $subj.$tokens() } else { Ok( v ) } )
      )*
     )
  )
);

fn is_ident_start_chr( chr : char ) -> bool {
  match chr {
    '_' => true,
    v   => v.is_alphabetic()
  }
}

fn is_ident_chr( chr : char ) -> bool {
  is_ident_start_chr( chr )
  || match chr {
    '\'' => true,
    v    => v.is_numeric()
  }
}

fn is_digit_char( chr : char ) -> bool {
  match chr {
    '0'...'9' => true,
    _ => false
  }
}

fn is_keyword( s : &str ) -> bool {
  match s {
    "if" | "let" | "then" | "else" | "in" | "def" | "where" | "type"
         | "data" => true,
    _ => false
  }
}

fn is_type_name( s : &str ) -> bool {
  s.char_at( 0 ).is_uppercase()
}

static MULTI_SYMBOL_CHRS : [char; 25] = [ '!', '#', '%', '&', '/', '=', '?'
                                         , '`', '´', '@', '$', '{', '}', '|'
                                         , '~', '^', '*', '<', '>', ',', '.'
                                         , ':', '-', '+',  '\\' ];

static SINGLE_SYMBOL_CHRS : [char; 4] = [ '(', ')', '[', ']' ];

fn is_symbol_chr( chr : char ) -> bool {
  is_multi_symbol_chr( chr ) || is_single_symbol_chr( chr )
}

fn is_multi_symbol_chr( chr : char ) -> bool {
  MULTI_SYMBOL_CHRS.contains( &chr )
}

fn is_single_symbol_chr( chr : char ) -> bool {
  SINGLE_SYMBOL_CHRS.contains( &chr )
}

impl<'a> Tokenizer<'a> {
  pub fn tokenize( src : &'a str, start_loc : CharLoc )
         -> TResult<Vec<Token>> {
    let mut tk = Tokenizer{ src         : src
                          , chars       : src.char_indices()
                          , start_loc   : start_loc
                          , current_chr: None
                          , current_loc : None
                          , checkpoints : Vec::new() };
    tk.start()
  }

  fn start( &mut self ) -> TResult<Vec<Token>> {
    let mut tokens = Vec::new();
    // Get the first non-whitespace character
    self.next();
    self.skip_whitespace( true );
    // While there's still characters left
    while !self.reached_end() {
      self.reset_checkpoints();
      // Try a fall-through parse of the different tokens
      let token = match fallthrough!( self : comment
                                           , ident
                                           , integer
                                           , symbol ) {
        Some( tk ) => tk,
        None => {
          return Err( TokenizerError::InvalidToken( self.current_loc.unwrap() ) )
        }
      };

      tokens.push( token );

      self.skip_whitespace( true );
    }
    // eof_loc is actually out of bounds for this file
    let eof_loc = self.start_loc.offset_by( CharOffset( self.src.len() as i32 ) );
    // Add the EOF token, which should always be the last
    tokens.push( Token::new( TokenKind::EOF, eof_loc ) );
    
    Ok( tokens.into_iter()
              // Filter out all the comments, since we don't need them
              .filter_map( |v| if !v.is_comment() {
                  Some( v )
                } else {
                  None
                } )
              .collect() )
  }

  fn comment( &mut self ) -> TResult<Option<Token>> {
    if try!( self.try_current() ) != ';' {
      return Ok( None )
    }

    let start_loc = self.current_loc.unwrap();
    
    // Skip the ';'
    self.next();

    let mut comment_content = String::new();
    
    // While we're not hitting a newline or EOF keep reading chars
    self.next_while( |_chr| {
      let chr = _chr.unwrap();
      if chr == '\n' {
        false
      } else {
        comment_content.push( chr );
        true
      }
    } );
    let token = Token::new( TokenKind::Comment( comment_content )
                          , start_loc );
    Ok( Some( token ) )
  }

  fn ident( &mut self ) -> TResult<Option<Token>> {
    if ! is_ident_start_chr( try!( self.try_current() ) ) {
      return Ok( None )
    }

    let start_loc = self.current_loc.unwrap();

    let mut ident_buffer = String::new();
    
    self.next_while( |_chr| {
      let chr = _chr.unwrap();
      if is_ident_chr( chr ) {
        ident_buffer.push( chr );
        true
      } else {
        false
      }
    } );

    let tkk = if is_keyword( &ident_buffer ) {
      TokenKind::Keyword( ident_buffer )

    } else if is_type_name( &ident_buffer ) {
      TokenKind::TypeName( ident_buffer )

    }else {
      TokenKind::Ident( ident_buffer )
    };
    
    let token = Token::new( tkk, start_loc );
    Ok( Some( token ) )
  }

  fn symbol( &mut self ) -> TResult<Option<Token>> {
    let chr = try!( self.try_current() );
    if ! is_symbol_chr( chr ) {
      return Ok( None )
    }

    let start_loc = self.current_loc.unwrap();

    let mut symbol_buffer = String::new();
    
    symbol_buffer.push( chr );
    // Go to the next char
    self.next();
    
    // If it's a valid multi-char symbol, keep building the token
    if is_multi_symbol_chr( chr ) {
      self.next_while( |_chr| {
        let chr = _chr.unwrap();
        if is_multi_symbol_chr( chr ) {
          symbol_buffer.push( chr );
          true
        } else {
          false
        }
      } );
    }
    
    let token = Token::new( TokenKind::Symbol( symbol_buffer )
                          , start_loc );

    Ok( Some( token ) )
  }

  fn integer( &mut self ) -> TResult<Option<Token>> {
    self.push_checkpoint();
    let mut chr = try!( self.try_current() );
    let start_loc = self.current_loc.unwrap();
    let mut intstr = String::new();
    
    match chr {
      '+' | '-' => {
        intstr.push( chr );
        self.next();
        chr = try!( self.try_current() );
      },
      _ => {}
    }
    
    if !is_digit_char( chr ) {
      self.pop_checkpoint();
      return Ok( None )
    }

    self.next_while( |_chr| {
      match _chr {
        Some( v ) => if is_digit_char( v ) {
          intstr.push( v );
          true
        } else {
          false
        },
        None => false
      }
    } );

    let ival = FromStr::from_str( &intstr )
                       .ok()
                       .expect( "Failed to read integer token" );
    Ok( Some( Token::new( TokenKind::Literal( Literal::Integer( ival ) )
                        , start_loc ) ) )
  }

  fn skip_whitespace( &mut self, newline : bool ) -> Option<char> {
    self.next_while( |_chr| {
      let chr = _chr.unwrap();
      match chr {
        // If the character is a non-newline whitespace, continue looping
        ' ' | '\r' | '\t' => {
          true
        },
        '\n' if newline => {
          true
        },
        // Else stop the loop
        _ => {
          false
        }
      }
    } )
  }
}

impl<'a> StreamReader<Option<char>, char, TokenizerError> for Tokenizer<'a> {
  // Gets the next character and updates the 
  fn next( &mut self ) -> Option<char> {
    // Get the next character if there is one,
    // else just return None.
    match self.chars.next() {
      Some( (idx, chr) ) => {
        self.current_chr = Some( chr );
        self.current_loc = Some( self.start_loc
                                     .offset_by( CharOffset( idx as i32 ) ) );
      },
      None => {
        self.current_chr = None;
        self.current_loc = None;
        return None
      }
    };

    self.current_chr
  }

  fn get_current( &self ) -> Option<char> {
    self.current_chr
  }

  fn set_current( &mut self, v : Option<char> ) {
    self.current_chr = v;
  }
  
  fn reached_end( &self ) -> bool {
    self.current_chr.is_none()
  }
  
  // Gets the current character, if none it returns an error
  fn try_current( &self ) -> TResult<char> {
    self.current_chr.ok_or( TokenizerError::UnexpectedEof )
  }
}

impl<'a> Checkpoint for Tokenizer<'a> {
  fn push_checkpoint( &mut self ) {
    self.checkpoints.push( (self.current_chr
                           , self.current_loc
                           , self.chars.clone()) );
  }

  fn pop_checkpoint( &mut self ) {
    match self.checkpoints.pop() {
      Some( (c, l, v) ) => {
        self.current_chr = c;
        self.current_loc = l;
        self.chars = v;
      },
      None => panic!( "Tried to pop a checkpoint from empty stack" )
    }
  }

  fn reset_checkpoints( &mut self ) {
    self.checkpoints.clear();
  }
}
