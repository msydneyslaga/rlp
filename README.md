# rl'

`rlp` (ruelang') will be a lazily-evaluated purely-functional language heavily
imitating Haskell.

### Architecture

![rlpc architecture diagram](/rlpc.drawio.svg)

### Build Info
* rlp is built using [Cabal](https://www.haskell.org/ghcup/)
* rlp's documentation is built using [Sphinx](https://www.sphinx-doc.org/en/master/)

```sh
$ cabal build       # Build the rlpc compiler
$ cabal install     # Install rlpc to $PATH
$ cabal haddock     # Build the API docs w/ Haddock
$ make -C doc html  # Build the primary docs w/ Sphinx

# run the test suite
$ cabal test --test-show-details=direct
```

### Use
```sh
# Compile and evaluate examples/factorial.hs, with evaluation info dumped to stderr
$ rlpc -ddump-eval examples/factorial.hs
# Compile and evaluate t.hs, with evaluation info dumped to t.log
$ rlpc -ddump-eval -l t.log t.hs
# Print the raw structure describing the compiler options
# (option parsing still must succeed in order to print)
$ rlpc -ddump-opts t.hs
```

### Potential Features
Listed in order of importance.
- [x] ADTs
- [x] First-class functions
- [ ] Higher-kinded types
- [ ] Typeclasses
- [x] Parametric polymorphism
- [x] Hindley-Milner type inference
- [ ] Newtype coercion
- [ ] Parallelism

### Milestones
(This list is incomplete.)

- [ ] Backend
    - [x] Core language
        - [x] AST
    - [x] Low-level execution model (TI)
        - [x] Arithmetic
        - [x] Conditionals
        - [x] Structured data
        - [x] Garbage collection
    - [x] Low-level execution model (GM)
        - [x] Arithmetic
        - [x] Conditionals
        - [x] Structured data
        - [x] Garbage Collection
    - [ ] Emitter
        - [ ] Code-gen (target yet to be decided)
    - [x] Core linter (Type-checker)
    - [ ] Core2Core pass (optimisations and misc. preprocessing)
        - [x] GM prep
            - [x] Non-strict case-floating
        - [ ] Let-floating
        - [ ] TCO
        - [ ] DCE
- [ ] Frontend
    - [x] High-level language
        - [x] AST
        - [x] Lexer
        - [x] Parser
    - [ ] Translation to the core language
        - [ ] Constraint solver
        - [ ] `do`-notation
    - [x] CLI
- [ ] Documentation
    - [x] State transition rules
    - [ ] How does the evaluation model work?
    - [ ] The Hindley-Milner type system
    - [ ] CLI usage
    - [ ] Tail call optimisation
    - [ ] Parsing rlp
    - [ ] Trees That Grow
- [ ] Tests
    - [x] Generic example programs
    - [ ] Parser

### ~~December Release Plan~~
- [x] Tests
    - [ ] Core lexer
    - [ ] Core parser
    - [x] Evaluation model
- [ ] Benchmarks
- [x] Stable Core lexer
- [x] Stable Core parser
- [x] Stable evaluation model
    - [x] Garbage Collection
- [ ] Stable documentation for the evaluation model

### January Release Plan
- [ ] Beta rl' to Core
- [ ] UX improvements
    - [x] Actual compiler errors -- no more unexceptional `error` calls
    - [x] Better CLI dump flags
    - [ ] Annotate the AST with token positions for errors (NOTE: As of Feb. 1,
      this has been done, but the locational info is not yet used in error messages)
- [ ] More examples

### March Release Plan
- [ ] Tests
    - [ ] rl' parser
    - [ ] rl' lexer

### Indefinite Release Plan

This list is more concrete than the milestones, but likely further in the future
than the other release plans.

- [ ] Stable rl' to Core
- [ ] Core polish
    - [ ] Better, stable parser
    - [ ] Better, stable lexer
    - [ ] Less hacky handling of named data
    - [ ] Less hacky pragmas
- [ ] GM to LLVM

