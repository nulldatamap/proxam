use std::collections::hash_map::{HashMap, Entry};

use filemap::{CharLoc};
use ast::{Function, Type, Expression};

// Highlevel Intermediate Code Representation

#[derive(Show)]
pub enum HicrError {
  UndeclaredFunction( Function ),
  AlreadyDeclaredFunction( Function, Function ),
  AlreadyDefinedFunction( Function, Function ),
  FunctionTypeNotMatching( Function, Function )
}

type HResult<T> = Result<T, HicrError>;

#[derive(Show)]
pub struct Module {
  pub functions : HashMap<String, Function>,
  // data structures
}

impl Module {
  fn new() -> Module {
    Module{ functions: HashMap::new() }
  }

  fn insert_fn( &mut self, fun : Function ) -> HResult<()> {
    let entry = self.functions.entry( fun.name.text.clone() );
    match entry {
      Entry::Vacant( va ) => { va.insert( fun ); },
      Entry::Occupied( mut oc ) => {
        match (&oc.get().body, &fun.body) {
          // In case we have to implementations
          (&Some( _ ), &Some( _ ))
            => return Err( HicrError::AlreadyDefinedFunction( fun
                                                            , oc.remove() ) ),
          // In case there's already an implementation 
          // and this is just a declaration
          (&Some( _ ), &None) => {
            if oc.get().ty != fun.ty {
              return Err( HicrError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
          },
          // In case there's a declaration and this is a implementation
          (&None, &Some( _ )) => {
            if oc.get().ty != fun.ty {
              return Err( HicrError::FunctionTypeNotMatching( fun
                                                            , oc.remove() ) )
            }
            oc.insert( fun );
          },
          // In case we have to delcarations
          (&None, &None) => {
            return Err( HicrError::AlreadyDeclaredFunction( fun
                                                          , oc.remove() ) ) 
          }
        }
      }
    }

    Ok( () )
  }
}

pub fn validate_module( fns : Vec<Function> ) -> HResult<Module> {
  let mut module = Module::new();
  
  for fun in fns.into_iter() {
    try!( module.insert_fn( fun ) );
  }
  
  Ok( module )
}


