Lexing, Parsing, and Layouts
============================

The C-style languages of my previous experiences have all had quite trivial
lexical analysis stages: you ignore all whitespace and point out the symbols you
recognise. If you don't recognise something, check if it's a literal or an
identifier. Should it be neither, return an error.

In contrast, both lexing and parsing a Haskell-like language poses a number of
greater challenges. Listed by ascending intimidation factor, some of the
potential roadblocks on my mind before making an attempt were:

* Context-sensitive keywords; Haskell allows for some words to be used as
  identifiers in appropriate contexts, such as :code:`family`, :code:`role`,
  :code:`as`. Reading a note_ found in `GHC's lexer`_, it appears that keywords
  are only considered in bodies for which their use is relevant, e.g.
  :code:`family` and :code:`role` in type declarations, :code:`as` after
  :code:`case`; :code:`if`, :code:`then`, and :code:`else` in expressions, etc.

* Operators; Haskell has not only user-defined infix operators, but user-defined
  precedence levels and associativities. I recall using an algorithm that looked
  up infix, prefix, postfix, and even mixfix operators up in a global table to
  call their appropriate parser (if their precedence was appropriate, also
  stored in the table). I never modified the table at runtime, however this
  could be a very nice solution for Haskell.

* Whitespace sensitivity; While I was comfortable with the idea of a system
  similar to Python's INDENT/DEDENT tokens, Haskell's layout system is based on
  alignment and is very generous with line-folding.

.. _note: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style#2-using-notes
.. _GHC's lexer: https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x#L1133

After a bit of thought and research, whitespace sensitivity in the form of
*layouts* as Haskell and I will refer to them as, are easily the scariest thing
on this list -- however they are achievable!

A Lexical Primer: Python
************************

We will compare and contrast with Python's lexical analysis. Much to my dismay,
Python uses newlines and indentation to separate statements and resolve scope
instead of the traditional semicolons and braces found in C-style languages (we
may generally refer to these C-style languages as *explicitly-sectioned*).
Internally during tokenisation, when the Python lexer encounters a new line, the
indentation of the new line is compared with that of the previous and the
following rules are applied:

1. If the new line has greater indentation than the previous, insert an INDENT
   token and push the new line's indentation level onto the indentation stack
   (the stack is initialised with an indentation level of zero).

2. If the new line has lesser indentation than the previous, pop the stack until
   the top of the stack is greater than the new line's indentation level. A
   DEDENT token is inserted for each level popped.

3. If the indentation is equal, insert a NEWLINE token to terminate the previous
   line, and leave it at that!

On the parser's end, the INDENT, DEDENT, and NEWLINE tokens are identical to
braces and semicolons. In developing our *layout* rules, we will follow in the
pattern of translating the whitespace-sensitive source language to an explicitly
sectioned language.

References
----------

* `Python's lexical analysis
  <https://docs.python.org/3/reference/lexical_analysis.html>`_

* `Haskell syntax reference
  <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html>`_

