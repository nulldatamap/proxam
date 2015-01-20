use std::collections::hash_map::{HashMap, Entry};

use filemap::{CharLoc};
use ast::{ModuleItem, FunctionBody, FunctionDecl, Type, Expression};

// Highlevel Intermediate Code Representation

#[derive(Show)]
pub enum HicrError {
  UndeclaredFunction( FunctionBody ),
  AlreadyDeclaredFunction( FunctionDecl, FunctionDecl ),
  AlreadyDefinedFunction( FunctionBody, FunctionBody ),
  FunctionTypeNotMatching( FunctionBody, FunctionDecl )
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

}

pub fn validate_module( mod_items : Vec<ModuleItem> ) -> HResult<Module> {
  let mut module = Module::new();
  let mut fn_table = HashMap::new();
  // First we build up a table of all the declations and bodies
  // This is the first place where we can fail, in case we try to
  // enter either a body or declaration twice.
  for mod_item in mod_items.into_iter() {
    try!( add_fn_entry( &mut fn_table, mod_item ) );
  }

  for (fn_name, decl_body) in fn_table.into_iter() {
    module.functions.insert( fn_name, try!( make_fn( decl_body ) ) );
  }

  Ok( module )
}

type FEntry = ( Option<FunctionDecl>, Option<FunctionBody> );

fn add_fn_entry( table : &mut HashMap<String, FEntry>, item : ModuleItem )
   -> HResult<()> {
  // Get the name and decl-body pair
  let (n, d, b) = match item {
    ModuleItem::FunctionDecl( decl ) => {
      (decl.name.text.clone(), Some( decl ), None)
    },
    ModuleItem::FunctionBody( body ) => {
      (body.name.text.clone(), None, Some( body ))
    }
  };
  // Calculate the new entry 
  let nval = {
    // If there's no entry for the function, just insert it
    let mut ent = match table.entry( n.clone() ) {
      Entry::Vacant( v ) => {
        v.insert( (d, b) );
        return Ok( () )
      },
      Entry::Occupied( ent ) => ent
    };
    // Else make sure we aren't overriding any existing bodies or declarations
    match (ent.remove(), d, b) {
      // Find out if we've already defined the function
      ((Some( orig ), _), Some( new ), _ ) => {
        return Err( HicrError::AlreadyDeclaredFunction( new, orig ) )
      },
      // Or if we've alredy declared it
      ((_, Some( orig )), _, Some( new )) => {
        return Err( HicrError::AlreadyDefinedFunction( new, orig ) )
      },
      // Insert the new declaration
      ((_, rest), Some( val ), _) => (Some( val ), rest),
      // Or definition into the pair
      ((rest, _), _, Some( val )) => (rest, Some( val )),
      // This should never happen
      inv => panic!( "Reached invalid pattern: {:?}", inv )
    }
  };
  // And insert the new pair
  table.insert( n, nval );
  Ok( () )
}

fn make_fn( decl_body : FEntry ) -> HResult<Function> {
  match decl_body {
    (None, Some( body )) => {
      return Err( HicrError::UndeclaredFunction( body ) )
    },
    (Some( decl ), obody) => {
      let arg_names;
      let fnbody = if let Some( body ) = obody {
        if decl.ty.argument_count() != body.args.len() as u32 {
          return Err( HicrError::FunctionTypeNotMatching( body, decl ) )
        }
        arg_names = body.args.into_iter()
                             .map( |v| v.text )
                             .collect();
        /*Some( decl.body )*/
        None
      } else {
        arg_names = range( 0, decl.ty.argument_count() )
                    .map( |i| format!( "_{}", i ) )
                    .collect();
        None
      };
      Ok( Function { ty: decl.ty
                   , arg_names: arg_names
                   , body: fnbody } )
    },
    inv => panic!( "Reached invalid pattern: {:?}", inv )
  }
}

#[derive(Show)]
pub struct Function {
  pub ty   : Type,
  pub arg_names : Vec<String>,
  pub body : Option<Expression>,
}

impl Function {

}


