# rl'

`rlp` (ruelang') will be a lazily-evaluated purely-functional language heavily
imitating Haskell.

### Build Info
```sh
$ cabal build       # build the rlpc compiler
$ cabal install     # install rlpc to $PATH
$ cabal haddock     # build the API docs w/ haddock
$ make -C docs html # build the primary docs w/ sphinx
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
        - [x] Garbage Collection
    - [x] Low-level execution model (GM)
        - [ ] Arithmetic
        - [ ] Conditionals
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
    - [ ] CLI
- [ ] Documentation (mostly for presentation)
    - [ ] State transition rules
    - [ ] How does the evaluation model work?
    - [ ] CLI usage
    - [ ] Tail call optimisation
    - [x] Parsing rlp
- [ ] Tests
    - [ ] Generic example programs
    - [ ] Parser

