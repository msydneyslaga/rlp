# rlp

`rlp` (ruelang') will be a lazily-evaluated purely-functional language heavily
imitating Haskell.

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
    - [ ] Low-level evaluation model (STG)
        - [x] Syntax
        - [x] Arithmetic
    - [ ] Emitter
        - [ ] Code-gen (target yet to be decided)
        - [ ] Core language emitter
    - [ ] Core linter (Type-checker)
    - [ ] Optimiser
        - [ ] Let-floating
        - [ ] TCO
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
    - [ ] How does an STG work?
    - [ ] CLI usage
    - [ ] Tail call optimisation
    - [x] Parsing rlp
- [ ] Tests
    - [ ] Generic example programs
    - [ ] Parser

