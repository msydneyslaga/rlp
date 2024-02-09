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

#### TLDR

```sh
# Compile and evaluate examples/factorial.cr, with evaluation info dumped to stderr
$ rlpc -ddump-eval examples/factorial.cr
# Compile and evaluate t.cr, with evaluation info dumped to t.log
$ rlpc -ddump-eval -l t.log t.cr
# Compile and evaluate t.rl, dumping the desugared Core
$ rlpc -ddump-desugared t.rl
```

#### Options

```sh
Usage: rlpc [-l|--log FILE] [-d DEBUG FLAG] [-f COMPILATION FLAG] 
            [-e|--evaluator gm|ti] [--heap-trigger INT] [-x|--language rlp|core]
            FILES...
```

Available debug flags include:
* `-ddump-desugared`: dump Core generated from rl'
* `-ddump-parsed-core`: dump raw Core AST
* `-ddump-parsed`: dump raw rl' AST
* `-ddump-eval`: dump evaluation logs
* `-dALL`: disable debug message filtering. enables **all** debug messages

### Potential Features
Listed in order of importance.
- [x] ADTs
- [x] First-class functions
- [x] Higher-kinded types
- [ ] Typeclasses
- [x] Parametric polymorphism
- [x] Hindley-Milner type inference
- [ ] Newtype coercion
- [ ] Parallelism

### Milestones
(This list is incomplete.)

Items are marked off not as they are 100% implemented, but rather once I
consider them stable enough that completion is soley a matter of getting
around to it -- no tough design decisions, theorising, etc. remain. For
example, as of writing this, the rl' frontend parser is not fully featured,
yet it is marked off on this list; finishing it would require cranking out
the remaining grammatical rules, and no work on complex tasks like layout
parsing remains.

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
    - [x] Translation to the core language
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

### February Release Plan
- [x] Beta rl' to Core
- [x] UX improvements
    - [x] Actual compiler errors -- no more unexceptional `error` calls
    - [x] Better CLI dump flags
    - [x] Annotate the AST with token positions for errors (NOTE: As of Feb. 1,
      this has been done, but the locational info is not yet used in error messages)
- [x] Compiler architecture diagram
- [ ] More examples

### March Release Plan
- [ ] Tests
    - [ ] rl' parser
    - [ ] rl' lexer

### Indefinite Release Plan

This list is more concrete than the milestones, but likely further in the future
than the other release plans.

- [ ] Overall codebase cleaning
    - [ ] Complete all TODOs
    - [ ] Replace mtl with effectful
- [ ] rl' type-checker
- [ ] Ditch TTG in favour of a simpler AST focusing on extendability via Fix, Free, 
  Cofree, etc. rather than boilerplate-heavy type families
- [ ] Stable rl' to Core
- [ ] Core polish
    - [ ] Better, stable parser
    - [ ] Better, stable lexer
    - [ ] Less hacky handling of named data
    - [ ] Less hacky pragmas
- [ ] Choose a target. LLVM, JS, C, and WASM are currently top contenders
- [ ] https://proglangdesign.net/wiki/challenges
