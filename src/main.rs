#![feature(rustc_private)]
// TOOD: Remove these warning suppressors
#![allow(unused_imports, unused_variables, unused_mut)]
#![feature(str_char, collections, slice_patterns)]

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

  let src_name = "<main>";
  let src_module_name = "main";

  let mut filemap = filemap::Filemap::new();
  let mut path = PathBuf::new();
  let pargs : Vec<String> = std::env::args().collect();
  // Try to read the file-name from the program arguments
  match &pargs[..] {
    [ _, ref n ] => {
      // Set the file path to be the one specified in the program arguments
      path.push( n );
    },
    _ => {
      println!( "Invalid arguments.\nExpected: proxam <file>" );
      return
    }
  }

  // First we register the input file into the file-map
  let fstart = match filemap.add_from_file( src_name.to_string()
                                          , path ) {
      Ok( s ) => s,
      Err( err ) => {
        println!( "Failed to add to the file map: {:?}", err );
        return
      }
  };

  // Then get it's file-map start location
  let fdes = filemap.get_charloc( fstart ).unwrap();

  // Start the tokenizer at our file's location
  let tks = match Tokenizer::tokenize( fdes.source, fstart ) {
    Ok( tks ) => tks,
    Err( err ) => {
      println!( "Failed to tokenize: {:?}", err );
      return
    }
  };

  // Proceed to parse the tokens
  let ast = match Parser::parse( src_name, fdes.source, &tks[..] ) {
    Ok( ast ) => ast,
    Err( err ) => {
      println!( "Failed to parse: {:?}", err );
      match err {
        // Report the error if it goes wrong
        parser::ParserError::SyntaxError( tk, _, _ ) => {
          let cld = filemap.get_charloc( tk.loc() ).unwrap();
          println!("At {}:{}", cld.line, cld.pos );
        },
        _ => {}
      }
      return
    }
  };

  // Validate and transform the module
  let module = match trans::validate_module( src_module_name.to_string()
                                           , ast ) {
    Ok( md ) => md,
    Err( err ) => {
      println!("Failed to validate module: {:?}", err );
      return
    }
  };

  // And generate the LLVM IR code 
  let llmodule = Codegen::generate_module( module );

  // Output the IR in human readable format
  unsafe {
    rustc::llvm::LLVMDumpModule( llmodule );
  }

} 
