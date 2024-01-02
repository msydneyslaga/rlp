The Complete Syntax of rl'
==========================

WIP.

Provided is the complete syntax of rl' in (pseudo) EBNF. {A} represents zero or
more A's, [A] means optional A, and terminals are wrapped in 'single-quotes'.

.. math
   :nowrap:

   \setlength{\grammarparsep}{20pt plus 1pt minus 1pt}
   \setlength{\grammarindent}{12em}
   \begin{grammar}
       <Decl> ::= <InfixDecl>
       \alt <DataDecl>
       \alt <TypeSig>
       \alt <FunDef>

       <InfixDecl> ::= <InfixWord> `litint' <Name>
       <InfixWord> ::= `infix'
       \alt `infixl'
       \alt `infixr'

       <DataDecl> ::= `data' `conname' {}

   \end{grammar}

.. code-block:: bnf

   Decl      ::= InfixDecl
               | DataDecl
               | TypeSig
               | FunDef

   InfixDecl ::= InfixWord 'litint' Operator
   InfixWord ::= 'infix'
               | 'infixl'
               | 'infixr'

   DataDecl  ::= 'data' 'conname' {'name'} '=' Data
   DataCons  ::= 'conname' {Type1} ['|' DataCons]

   TypeSig   ::= Var '::' Type
   FunDef    ::= Var {Pat1} '=' Expr

   Type      ::= Type1 {Type1}
               -- note that (->) is right-associative,
               -- and extends as far as possible
               | Type '->' Type
   Type1     ::= '(' Type ')'
               | 'conname'

   Pat       ::= 'conname' Pat1 {Pat1}
               | Pat 'consym' Pat

   Pat1      ::= Literal
               | 'conname'
               | '(' Pat ')'

   Literal   ::= 'litint'

   Var ::= 'varname'
         | '(' 'varsym' ')'
   Con ::= 'conname'
         | '(' 'consym' ')'

