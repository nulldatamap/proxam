use std::convert::From;
use std::path::{PathBuf, Path};
use std::io::Error as IoError;
use std::io::Read;
use std::fs::File;

#[derive(Debug, PartialEq, Eq, Copy, Clone, Hash)]
// Represents a characters location in the filemap
pub struct CharLoc( pub u32 );

impl CharLoc {
  pub fn as_u32( &self ) -> u32 {
    let CharLoc( s ) = *self;
    s
  }

  pub fn offset_by( &self, off : CharOffset ) -> CharLoc {
    let offset = self.as_u32() as i32 + off.as_i32();
    CharLoc( offset as u32 )
  }

  pub fn offset_of( &self, other : CharLoc ) -> CharOffset {
    let dif = other.as_u32() as i32 - self.as_u32() as i32;
    CharOffset( dif )
  }

  pub fn inbetween( &self, startp : CharLoc, endp : CharOffset ) -> bool {
    let v = self.as_u32();
    let end = startp.as_u32() + endp.as_i32() as u32;
    
    v >= startp.as_u32() && v < end
  }
}

#[derive(Debug, PartialEq, Eq, Copy, Clone)]
// Represents a character offset in the filemap
pub struct CharOffset( pub i32 );

impl CharOffset {
  pub fn as_i32( &self ) -> i32 {
    let CharOffset( s ) = *self;
    s
  }
}

pub trait Loc {
  fn loc( &self ) -> CharLoc;
}

// Creates a vector of offset-length pair for all the lines in a string
fn read_lines_char_count( src : &str ) -> Vec<(u32, u32)> {
  let mut sum = 0;
  src.lines()
     .map( |v| {
        let r = (v.len() as u32 + 1, sum);
        sum += v.len() as u32 + 1;
        r
      } )
     .collect()
}

// The representation of an entry into the filemap
struct FilemapEntry {
  // Where the original file is location ( if any )
  path : Option<PathBuf>,
  // What the name of the entry is
  name : String,
  // What it's source conent is
  source : String,
  // It's offset-length pairs of the lines
  lines : Vec<(u32, u32)>,
  // What the entry's starting location in the filemap is
  start_loc : CharLoc,
  // Where the entry ends in the filemap
  end_loc : CharOffset
}

impl FilemapEntry {
  fn new( name : String, path : Option<PathBuf>, source : String
        , start_loc : CharLoc ) -> FilemapEntry {
    let lines = read_lines_char_count( &source[..] );
    FilemapEntry { path     : path
                 , name     : name
                 , end_loc  : CharOffset( source.len() as i32 )
                 , source   : source
                 , lines    : lines
                 , start_loc: start_loc }
  }

  // Get's an immutable descriptor ( a reference ) to the entry
  fn as_descriptor<'a>( &'a self, off : CharOffset ) -> LocDescriptor<'a> {
    let (line, pos) = self.get_line_pos( off );
    LocDescriptor { path  : self.path.as_ref().map( |v| {
                        let r : &Path = &v;
                        r
                      } )
                  , name  : &self.name[..]
                  , source: &self.source[..]
                  , line  : line
                  , pos   : pos }
  }

  // Get's the line and position of a given char in the entry
  fn get_line_pos( &self, off : CharOffset ) -> (u32, u32) {
    let offv = off.as_i32();
    let mut line = 1;
    for &(width, start) in self.lines.iter() {
      let pos = offv as u32 - start;
      if pos < width {
        return (line, 1 + pos)
      }
      line += 1;
    }
    panic!( " CharOffset out of bounds." )
  }
}

#[derive(Debug)]
pub enum FileMapError {
  LocitionOutOfBounds,
  IoError( IoError )
}

impl From<IoError> for FileMapError {
  fn from( err : IoError ) -> FileMapError {
    FileMapError::IoError( err )
  }
}

#[derive(Debug)]
// A descriptor used to describe the file entry which a given character lies in
pub struct LocDescriptor<'a> {
  pub path : Option<&'a Path>,
  pub name : &'a str,
  pub source : &'a str,
  pub line : u32,
  pub pos : u32
}

// The registry for all source files
pub struct Filemap {
  files : Vec<FilemapEntry>,
  high_bound : CharLoc
}

impl Filemap {
  pub fn new() -> Filemap {
    Filemap { files: Vec::new()
            , high_bound: CharLoc( 0 ) }
  }

  // Adds a file to the registry
  pub fn add_from_file( &mut self, name : String, path : PathBuf )
     -> Result<CharLoc, FileMapError> {
    let mut file = try!( File::open( &path ) );
    let mut source = String::new();
    let _ = try!( file.read_to_string( &mut source ) );
    let fme = FilemapEntry::new( name, Some( path ), source, self.high_bound );

    self.high_bound = self.high_bound.offset_by( fme.end_loc );

    let r = fme.start_loc;

    self.files.push( fme );

    Ok( r )
  }

  // Tries to get a descriptor for a given character in the filemap
  pub fn get_charloc<'a>( &'a self, loc : CharLoc )
     -> Result<LocDescriptor<'a>, FileMapError> {
    let entry = try!( self.lookup_charloc( loc ) );

    Ok( entry.as_descriptor( entry.start_loc.offset_of( loc ) ) )
  }

  fn lookup_charloc( &self, loc : CharLoc )
     -> Result<&FilemapEntry, FileMapError> {
    let CharLoc( mx ) = self.high_bound;
    let CharLoc( p ) = loc;

    if p >= mx {
      return Err( FileMapError::LocitionOutOfBounds )
    }

    Ok( self.files.iter()
                  .find( |v| loc.inbetween( v.start_loc, v.end_loc ) )
                  .expect( "The given CharLoc isn't inside any of the mapped files." ) )

  }

}
