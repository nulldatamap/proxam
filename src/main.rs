#![feature(rustc_private)]

#[macro_use]
extern crate version;
extern crate rustc;

use filemap::Loc;
use tokenizer::Tokenizer;
use parser::Parser;
use codegen::Codegen;

mod filemap;
#[macro_use]
mod streamreader;
mod tokenizer;
mod parser;
mod ast;
mod visitor;
mod folder;
mod trans;
mod builtin;
mod codegen;

#[cfg(test)]
mod test;

fn main() {
  use std::path::PathBuf;

  println!( "Proxam compiler v{}", version!() );

  let test_name = "<test>";
  let test_module_name = "helloworld";

  let mut filemap = filemap::Filemap::new();
  let fstart = match filemap.add_from_file( test_name.to_string()
                                          , PathBuf::new( "src/testsrc.pxm" ) ) {
      Ok( s ) => s,
      Err( err ) => {
        println!( "Failed to add to the file map: {:?}", err );
        return
      }
  };

  let fdes = filemap.get_charloc( fstart ).unwrap();

  let tks = match Tokenizer::tokenize( fdes.source, fstart ) {
    Ok( tks ) => tks,
    Err( err ) => {
      println!( "Failed to tokenize: {:?}", err );
      return
    }
  };

  let ast = match Parser::parse( test_name, fdes.source, tks.as_slice() ) {
    Ok( ast ) => ast,
    Err( err ) => {
      println!( "Failed to parse: {:?}", err );
      match err {
        parser::ParserError::SyntaxError( tk, _, _ ) => {
          let cld = filemap.get_charloc( tk.loc() ).unwrap();
          println!("At {}:{}", cld.line, cld.pos );
        },
        _ => {}
      }
      return
    }
  };

  //println!( "=> {:?}", ast );

  let module = match trans::validate_module( test_module_name.to_string()
                                           , ast ) {
    Ok( md ) => md,
    Err( err ) => {
      println!("Failed to validate module: {:?}", err );
      return
    }
  };

  println!("=> {:?}", module );

  let llmodule = Codegen::generate_module( module );

  unsafe {
    rustc::llvm::LLVMDumpModule( llmodule );
  }

} 
