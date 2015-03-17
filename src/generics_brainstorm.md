
# BRAINSTORM 
## Generics!
### Data structures
- Currently all functions have a list of constraints
  ( which are of type `Vec<Class>` ) 

### Source to data mapping
Our test case:
```proxam
; T, being a generic type value
def eq a b : T, T -> Bool = 
  ...
```

Since there's no constraints on the generic value, none will be represented
```rust
Function {
    ...
    constraints: [] // No constraints
}

```
The type of both `a` and `b` will be `T`, represnted as:
```rust
Generic( "T", [] )
```


### Resolution
### Constraints
### Specification
How will generic functions get their specified versions generated?



### Inference


