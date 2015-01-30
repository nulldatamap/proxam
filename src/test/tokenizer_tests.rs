
/* DELETE THIS
COMMENT := ";" (-"\n")* "\n"
IDENT := (<is_alphanumeric_lower> | "_")(<is_alphanumeric> | "_" | "'")*
TYPE_NAME := (<is_alphanumeric_upper> | "_")(<is_alphanumeric> | "_" | "'")*
SYMBOL := SINGLE_SYMBOL | MULTI_SYMBOL+
KEYWORD = "if" | "then" | "else" | "let" | "in" | "def"
SINGLE_SYMBOL := "(" | ")" | "[" | "]"
MULTI_SYMBOL := "!" | "#" | "%" | "&" | "/" | "=" | "?" | "`" | "´" | "@" | "$" | "{" | "}" | "|" | "~" | "^" | "*" | "<" | ">" | "," | "." | ":" | "-" | "\\"
DIGIT := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
INTEGER := ("+" | "-")? DIGIT+
BOOLEAN := "True" | "False"
*/

static test00 : &'static str = "";

#[test]
fn check_positions() {
  
}
