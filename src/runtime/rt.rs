
#[no_mangle]
extern "C" fn print_int( v : i32 ) {
  println!( "{}", v )
}
