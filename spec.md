
# Proxam Specification

## Tokens

```antlr
COMMENT := ";" (-"\n")* "\n"
IDENT := (<is_alphanumeric> | "_")(<is_alphanumeric> | "_" | "'")*
SYMBOL := SINGLE_SYMBOL | MULTI_SYMBOL+
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
item := function_body
function_body := IDENT+ "=" expression
function_decl := IDENT ":" type
type := function_type | _type
_type := builtin_type
builtin_type := "int"
function_type := _type "->" type
expression := "!"
```

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

