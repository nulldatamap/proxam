
pub trait StreamReader<T, R, E> {
  fn next( &mut self ) -> T;
  fn set_current( &mut self, T );
  fn get_current( &self ) -> T;

  fn next_while<F>( &mut self, mut f : F ) -> T
     where F: FnMut( T ) -> bool {
    while !self.reached_end() {
      if !f( self.get_current() ) {
        break
      }
      self.next();
    }
    self.get_current()
  }

  fn reached_end( &self ) -> bool;
  fn try_current( &self ) -> Result<R, E>;
}

pub trait Checkpoint {
  fn push_checkpoint( &mut self );
  fn pop_checkpoint( &mut self );
  fn reset_checkpoints( &mut self );
}

#[macro_export]
macro_rules! fallthrough(
  ( $subj:ident : $token:ident, $($tokens:ident),* ) => (
    try!( $subj.$token()
      $(
        .and_then( |v| if v.is_none() { $subj.$tokens() } else { Ok( v ) } )
      )*
     )
  )
);

