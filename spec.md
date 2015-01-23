
# Proxam Specification

## Tokens

```antlr
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
```

Not yet implemented:
```antlr
FLOAT := DIGIT "." DIGIT?
```

## Syntax:
```antlr
module := function*
def := "def" function
function := ident (ident)* ":" type ("=" expression)?
type := function_type
function_type := _type ("," _type)* "->" type
_type := "(" type ")" | TYPE_NAME
expression := appl_expr
appl_expr := op_expr n (op_expr n)*
op_expr (x <- 1..n) := op_expr (x - 1) <op:SYMBOL x> op_expr (x - 1)
op_expr 0 := low_expr <op 0> low_expr
low_expr = "(" expression ")" | if_expr | let_expr | IDENT | literal
if_expr = "if" expression "then" expression "else" expression
let_expr = "let" function ("," function)* "in" expression
literal = INTEGER
```

`op_expr x` referring to the infix operator(s) with a precedence of `x`.
`op_expr (x <- 1..n) := op_expr (x - 1) <op x> op_expr (x - 1)` meaning that the definition of operator number `x` is a pair of a decrement lower operators ( or `low_expr` in the case of `op_expr 1` ) surrounding the symbol for the given operator.

Not yet implemented:
```antlr
type := function_type | _type | "(" type ")"
_type := builtin_type | list_type | tuple_type
function_type := type ("," type)* "->" type
list_type := "[" type "]"
tuple_type := "(" ( type ",")*  ")"
```

# Required documentation:
* [ ] Design goals
* [ ] Architecture
* [ ] Module summary
  - [ ] main
  - [ ] filemap
  - [ ] streamreader
  - [ ] tokenizer
  - [ ] parser
  - [ ] ast
  - [ ] hicr
  - [ ] codegen
* [ ] Data type documentations
  - [ ] main
  - [ ] filemap
  - [ ] streamreader
  - [ ] tokenizer
  - [ ] parser
  - [ ] ast
  - [ ] hicr
  - [ ] codegen
* In-code documentation
  - [ ] main
  - [ ] filemap
  - [ ] streamreader
  - [ ] tokenizer
  - [ ] parser
  - [ ] ast
  - [ ] hicr
  - [ ] codegen

# Required tests:
* [ ] (StreamReader?)
* [ ] FileMap
* [ ] Tokenizer
* [ ] Parser
* [ ] Hicr
* [ ] Codegen

