# rl'

`rlp` (ruelang') will be a lazily-evaluated purely-functional language heavily
imitating Haskell.

### Build Info
* rlp is built using [Cabal](https://www.haskell.org/ghcup/)
* rlp's documentation is built using [Sphinx](https://www.sphinx-doc.org/en/master/)

```sh
$ cabal build       # Build the rlpc compiler
$ cabal install     # Install rlpc to $PATH
$ cabal haddock     # Build the API docs w/ Haddock
$ make -C doc html  # Build the primary docs w/ Sphinx
```

### Use
```sh
# Compile and evaluate t.hs
$ rlpc t.hs
# Compile and evaluate t.hs, with evaluation info dumped to stderr
$ rlpc -ddump-eval t.hs
# Compile and evaluate t.hs, with evaluation info dumped to t.log
$ rlpc -ddump-eval -l t.log t.hs
# Print the raw structure describing the compiler options and die
# (option parsing still must succeed in order to print)
$ rlpc -ddump-opts t.hs
```

### Potential Features
Listed in order of importance.
- [ ] ADTs
- [ ] First-class functions
- [ ] Higher-kinded types
- [ ] Typeclasses
- [ ] Parametric polymorphism
- [ ] Hindley-Milner type inference
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
    - [ ] Low-level execution model (GM)
        - [x] Arithmetic
        - [x] Conditionals
        - [x] Structured data
        - [ ] Garbage Collection
    - [ ] Emitter
        - [ ] Code-gen (target yet to be decided)
        - [ ] Core language emitter
    - [ ] Core linter (Type-checker)
    - [ ] Optimisation pass
        - [ ] Let-floating
        - [ ] TCO
        - [ ] DCE
- [ ] Frontend
    - [ ] High-level language
        - [ ] AST
        - [ ] Lexer
        - [ ] Parser
    - [ ] Translation to the core language
        - [ ] Constraint solver
        - [ ] `do`-notation
        - [ ] Renamer
    - [ ] CLI
- [ ] Documentation
    - [ ] State transition rules
    - [ ] How does the evaluation model work?
    - [ ] CLI usage
    - [ ] Tail call optimisation
    - [x] Parsing rlp
- [ ] Tests
    - [ ] Generic example programs
    - [ ] Parser

### December Release Plan
- [ ] Tests
    - [ ] Core lexer
    - [ ] Core parser
    - [ ] Evaluation model
- [ ] Benchmarks
- [ ] Stable Core lexer
- [ ] Stable Core parser
- [ ] Stable evaluation model
    - [ ] Garbage Collection
- [ ] Stable documentation for the evaluation model
