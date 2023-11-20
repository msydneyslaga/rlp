Lexing, Parsing, and Layouts
============================

The C-style languages of my previous experiences have all had quite trivial
lexical analysis stages, peaking in complexity when I streamed tokens lazily in
C. The task of tokenising a C-style language is very simple in description: you
ignore all whitespace and point out what you recognise. If you don't recognise
something, check if it's a literal or an identifier. Should it be neither,
return an error.

On paper, both lexing and parsing a Haskell-like language seem to pose a few
greater challenges. Listed by ascending intimidation factor, some of the
potential roadblocks on my mind before making an attempt were:

* Operators; Haskell has not only user-defined infix operators, but user-defined
  precedence levels and associativities. I recall using an algorithm that looked
  up infix, prefix, postfix, and even mixfix operators up in a global table to
  call their appropriate parser (if their precedence was appropriate, also
  stored in the table). I never modified the table at runtime, however this
  could be a very nice solution for Haskell.

* Context-sensitive keywords; Haskell allows for some words to be used as identifiers in
  appropriate contexts, such as :code:`family`, :code:`role`, :code:`as`.
  Reading a note_ found in `GHC's lexer
  <https://gitlab.haskell.org/ghc/ghc/-/blob/master/compiler/GHC/Parser/Lexer.x#L1133>`_,
  it appears that keywords are only considered in bodies for which their use is
  relevant, e.g. :code:`family` and :code:`role` in type declarations,
  :code:`as` after :code:`case`; :code:`if`, :code:`then`, and :code:`else` in
  expressions, etc.

* Whitespace sensitivity; While I was comfortable with the idea of a system
  similar to Python's INDENT/DEDENT tokens, Haskell seemed to use whitespace to
  section code in a way that *felt* different.

.. _note: https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/coding-style#2-using-notes

After a bit of thought and research, whitespace sensitivity in the form of
*layouts* as Haskell and I will refer to them as, are easily the scariest thing
on this list -- however they are achievable!

A Lexical Primer: Python
************************

We will compare and contrast with Python's lexical analysis. Much to my dismay,
Python uses newlines and indentation to separate statements and resolve scope
instead of the traditional semicolons and braces found in C-style languages (we
may generally refer to these C-style languages as *explicitly-sectioned*).
Internally during tokenisation, when the Python lexer begins a new line, they
compare the indentation of the new line with that of the previous and apply the
following rules:

1. If the new line has greater indentation than the previous, insert an INDENT
   token and push the new line's indentation level onto the indentation stack
   (the stack is initialised with an indentation level of zero).

2. If the new line has lesser indentation than the previous, pop the stack until
   the top of the stack is greater than the new line's indentation level. A
   DEDENT token is inserted for each level popped.

3. If the indentation is equal, insert a NEWLINE token to terminate the previous
   line, and leave it at that!

Parsing Python with the INDENT, DEDENT, and NEWLINE tokens is identical to
parsing a language with braces and semicolons. This is a solution pretty in line
with Python's philosophy of the "one correct answer" (TODO: this needs a
source). In developing our *layout* rules, we will follow in the pattern of
translating the whitespace-sensitive source language to an explicitly sectioned
language.

But What About Haskell?
***********************

We saw that Python, the most notable example of an implicitly sectioned
language, is pretty simple to lex. Why then am I so afraid of Haskell's layouts?
To be frank, I'm far less scared after asking myself this -- however there are
certainly some new complexities that Python needn't concern. Haskell has
implicit line *continuation*: forms written over multiple lines; indentation
styles often seen in Haskell are somewhat esoteric compared to Python's
"s/[{};]//".

.. code-block:: haskell

   -- line continuation
   something = this is a
       single expression

   -- an extremely common style found in haskell
   data Python = Users
       { are        :: Crying
       , right      :: About
       , now        :: Sorry
       }

   -- another formatting oddity
   -- note that this is not line contiation!
   -- `look at`, `this`, and `alignment`
   -- are all separate expressions!
   anotherThing = do look at
                     this
                     alignment

But enough fear, lets actually think about implementation. Firstly, some
formality: what do we mean when we say layout? We will define layout as the
rules we apply to an implicitly-sectioned language in order to yield one that is
explicitly-sectioned. We will also define indentation of a lexeme as the column
number of its first character.

Thankfully for us, our entry point is quite clear; layouts only appear after a
select few keywords, (with a minor exception; TODO: elaborate) being :code:`let`
(followed by supercombinators), :code:`where` (followed by supercombinators),
:code:`do` (followed by expressions), and :code:`of` (followed by alternatives)
(TODO: all of these terms need linked glossary entries). Under this assumption,
we give the following rule:

1. If a :code:`let`, :code:`where`, :code:`do`, or :code:`of` keyword is not
   followed by the lexeme :code:`{`, the token :math:`\{n\}` is inserted after
   the keyword, where :math:`n` is the indentation of the next lexeme if there
   is one, or 0 if the end of file has been reached.

Henceforth :math:`\{n\}` will denote the token representing the begining of a
layout; similar in function to a brace, but it stores the indentation level for
subsequent lines to compare with. We must introduce an additional input to the
function handling layouts. Obviously, such a function would require the input
string, but a helpful book-keeping tool which we will make good use of is a
stack of "layout contexts", describing the current cascade of layouts. Each
element is either a :code:`NoLayout`, indicating an explicit layout (i.e. the
programmer inserted semicolons and braces herself) or a :code:`Layout n` where
:code:`n` is a non-negative integer representing the indentation level of the
enclosing context.

.. code-block:: haskell

    f x -- layout stack: []
        = let -- layout keyword; remember indentation of next token
              y = w * w -- layout stack: [Layout 10]
              w = x + x
          in do -- layout keyword; next token is a brace!
              { -- layout stack: [NoLayout]
              pure }

In the code seen above, notice that :code:`let` allows for multiple definitions,
separated by a newline. We accomate for this with a token :math:`\langle n
\rangle` which compliments :math:`\{n\}` in how it functions as a closing brace
that stores indentation. We give a rule to describe the source of such a token:

2. When the first lexeme on a line is preceeded by only whitespace a
   :math:`\langle n \rangle` token is inserted before the lexeme, where
   :math:`n` is the indentation of the lexeme, provided that it is not, as a
   consequence of rule 1 or rule 3 (as we'll see), preceded by {n}. 

Lastly, to handle the top level we will initialise the stack with a
:math:`\{n\}` where :math:`n` is the indentation of the first lexeme.

3. If the first lexeme of a module is not '{' or :code:`module`, then it is
   preceded by :math:`\{n\}` where :math:`n` is the indentation of the lexeme. 

For a more pedantic description of the layout system, see `chapter 10
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html>`_ of the
2010 Haskell Report, which I **heavily** referenced here.

References
----------

* `Python's lexical analysis
  <https://docs.python.org/3/reference/lexical_analysis.html>`_

* `Haskell Syntax Reference
  <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html>`_
