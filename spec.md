
# Proxam Specification

## Tokens

```antlr
COMMENT := ";" (-"\n")* "\n"
IDENT := (<is_alphanumeric_lower> | "_")(<is_alphanumeric> | "_" | "'")*
TYPE_NAME := (<is_alphanumeric_upper> | "_")(<is_alphanumeric> | "_" | "'")*
SYMBOL := SINGLE_SYMBOL | MULTI_SYMBOL+
KEYWORD = "if" | "then" | "else" | "let" | "in" | "def" | "where"
SINGLE_SYMBOL := "(" | ")" | "[" | "]"
MULTI_SYMBOL := "!" | "#" | "%" | "&" | "/" | "=" | "?" | "`" | "´" | "@" | "$" | "{" | "}" | "|" | "~" | "^" | "*" | "<" | ">" | "," | "." | ":" | "-" | "\\"
DIGIT := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
INTEGER := ("+" | "-")? DIGIT+
BOOLEAN := "True" | "False"
```

Not yet implemented:
```antlr
FLOAT := DIGIT "." DIGIT?
```

## Syntax:
```antlr
module := item*
item := fn_def | type_def | data_def
fn_def := "def" function
function := ident (ident)* ":" type where? ("=" expression)?
type := function_type
function_type := _type ("," _type)* "->" type
_type := tuple_type | list_type | TYPE_NAME | IDENT
tuple_type := "(" ")" | "(" type ")" | "(" (type ",")+ ")"
list_type := "[" type "]"
where := "where" type_constraint ("," type_constraint)*
type_constraint := TYPE_NAME type+
expression := appl_expr
appl_expr := op_expr n (op_expr n)*
op_expr (x <- 0..(n - 1)) := op_expr (x + 1) <op:SYMBOL x> op_expr (x + 1)
op_expr n := low_expr <op 0> low_expr
low_expr := "(" expression ")" | if_expr | let_expr | IDENT | literal
if_expr := "if" expression "then" expression "else" expression
let_expr := "let" function ("," function)* "in" expression
literal := INTEGER
type_def := "type" TYPE_NAME "=" type
data_def := "data" TYPE_NAME "=" type | ( IDENT ":" type ("," IDENT ":" type)* )
```

`op_expr x` referring to the infix operator(s) with a precedence of `x`.
`op_expr (x <- 1..n) := op_expr (x - 1) <op x> op_expr (x - 1)` meaning that the definition of operator number `x` is a pair of a decrement lower operators ( or `low_expr` in the case of `op_expr n`, the highest precedence operator(s) ) surrounding the symbol for the given operator(s).

