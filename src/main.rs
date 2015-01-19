#[macro_use]
extern crate version;
extern crate rustc;

use tokenizer::Tokenizer;
use parser::Parser;

mod filemap;
#[macro_use]
mod streamreader;
mod tokenizer;
mod parser;
mod ast;
mod hicr;
mod codegen;

fn main() {
  println!( "Proxam compiler v{}", version!() );

  let test_src = "\
magic : !\n\
magic x y z = !";
  let test_name = "<test>";
  let test_module_name = "helloworld";

  let mut filemap = filemap::Filemap::new();
  let fstart = match filemap.add_from_string( test_name.to_string()
                                            , test_src.to_string() ) {
    Ok( s ) => s,
    Err( err ) => {
      println!( "Failed to add to the file map: {:?}", err );
      return
    }
  };

  let tks = match Tokenizer::tokenize( test_src, fstart ) {
    Ok( tks ) => tks,
    Err( err ) => {
      println!( "Failed to tokenize: {:?}", err );
      return
    }
  };

  let ast = match Parser::parse( test_name, test_src, tks.as_slice() ) {
    Ok( ast ) => ast,
    Err( err ) => {
      println!( "Failed to parse: {:?}", err );
      return
    }
  };

  println!( "=> {:?}", ast );

  let module = match hicr::validate_module( ast ) {
    Ok( md ) => md,
    Err( err ) => {
      println!("Failed to validate module: {:?}", err );
      return
    }
  };

  let llmodule = codegen::generate_module( test_module_name, module );

  unsafe {
    rustc::llvm::LLVMDumpModule( llmodule );
  }

} 
