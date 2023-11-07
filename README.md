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
- [ ] Newtypes
- [ ] Parallelism
- [ ] C# interop

### Milestones
(This list is incomplete.)

- [ ] Backend
    - [ ] Core language
        - [ ] AST
    - [ ] Emitter
        - [ ] MSIL Codegen module
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

