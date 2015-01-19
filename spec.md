
# Proxam Specification

## Tokens

```antlr
COMMENT := ";" (-"\n")* "\n"
IDENT := (<is_alphanumeric> | "_")(<is_alphanumeric> | "_" | "'")*
SYMBOL := SINGLE_SYMBOL | MULTI_SYMBOL+
SINGLE_SYMBOL := "(" | ")" | "[" | "]"
MULTI_SYMBOL := "!" | "#" | "%" | "&" | "/" | "=" | "?" | "`" | "´" | "@" | "$" | "{" | "}" | "|" | "~" | "^" | "*" | "<" | ">" | "," | "." | ":" | "-" | "\\"
```

Not yet implemented:
```antlr
DIGIT := "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
NUMBER := ("+" | "-")? DIGIT+
FLOAT := DIGIT "." DIGIT?
```

## Syntax:
```antlr
module := item*
item := function_body
function_body := IDENT+ "=" expression
function_decl := IDENT ":" type
type := "!"
expression := "!"
```

Not yet implemented:
```antlr
type := function_type | _type
_type := IDENT | list_type | tuple_type
function_type := _type "->" type
list_type := "[" type "]"
tuple_type := "(" type (("," type)* | "," ) ")"
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

