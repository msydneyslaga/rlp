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
  Reading a note_ found in `GHC's lexer`_,
  it appears that keywords are only considered in bodies for which their use is
  relevant, e.g. :code:`family` and :code:`role` in type declarations,
  :code:`as` after :code:`case`; :code:`if`, :code:`then`, and :code:`else` in
  expressions, etc.

* Whitespace sensitivity; While I was comfortable with the idea of a system
  similar to Python's INDENT/DEDENT tokens, Haskell seemed to use whitespace to
  section code in a way that *felt* different.

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
   -- note that this is not a single
   -- continued line! `look at`,
   -- `this`, and `alignment` are all
   -- separate expressions!
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
(TODO: all of these terms need linked glossary entries). In order to manage the
cascade of layout contexts, our lexer will record a stack for which each element
is either :math:`\varnothing`, denoting an explicit layout written with braces
and semicolons, or a :math:`\langle n \rangle`, denoting an implicitly laid-out
layout where the start of each item belonging to the layout is indented
:math:`n` columns.

.. code-block:: haskell

    -- layout stack: []
    module M where -- layout stack: [∅]

    f x = let -- layout keyword; remember indentation of next token
              y = w * w -- layout stack: [∅, <10>]
              w = x + x
              -- layout ends here
          in do -- layout keyword; next token is a brace!
              { -- layout stack: [∅]
                  print y;
                  print x;
              }

Finally, we also need the concept of "virtual" brace tokens, which as far as
we're concerned at this moment are exactly like normal brace tokens, except
implicitly inserted by the compiler. With the presented ideas in mind, we may
begin to introduce a small set of informal rules describing the lexer's handling
of layouts, the first being:

1. If a layout keyword is followed by the token '{', push :math:`\varnothing`
   onto the layout context stack. Otherwise, push :math:`\langle n \rangle` onto
   the layout context stack where :math:`n` is the indentation of the token
   following the layout keyword. Additionally, the lexer is to insert a virtual
   opening brace after the token representing the layout keyword.

Consider the following observations from that previous code sample:

* Function definitions should belong to a layout, each of which may start at
  column 1.

* A layout can enclose multiple bodies, as seen in the :code:`let`-bindings and
  the :code:`do`-expression.

* Semicolons should *terminate* items, rather than *separate* them.

Our current focus is the semicolons. In an implicit layout, items are on
separate lines each aligned with the previous. A naïve implementation would be
to insert the semicolon token when the EOL is reached, but this proves unideal
when you consider the alignment requirement. In our implementation, our lexer
will wait until the first token on a new line is reached, then compare
indentation and insert a semicolon if appropriate. This comparison -- the
nondescript measurement of "more, less, or equal indentation" rather than a
numeric value -- is referred to as *offside* by myself internally and the
Haskell report describing layouts. We informally formalise this rule as follows:

2. When the first token on a line is preceeded only by whitespace, if the
   token's first grapheme resides on a column number :math:`m` equal to the
   indentation level of the enclosing context -- i.e. the :math:`\langle n
   \rangle` on top of the layout stack. Should no such context exist on the
   stack, assume :math:`m > n`.

We have an idea of how to begin layouts, delimit the enclosed items, and last
we'll need to end layouts. This is where the distinction between virtual and
non-virtual brace tokens comes into play. The lexer needs only partial concern
towards closing layouts; the complete responsibility is shared with the parser.
This will be elaborated on in the next section. For now, we will be content with
naïvely inserting a virtual closing brace when a token is indented right of the
layout.

3. Under the same conditions as rule 2., when :math:`m < n` the lexer shall
   insert a virtual closing brace and pop the layout stack.

This rule covers some cases including the top-level, however, consider
tokenising the :code:`in` in a :code:`let`-expression. If our lexical analysis
framework only allows for lexing a single token at a time, we cannot return both
a virtual right-brace and a :code:`in`. Under this model, the lexer may simply
pop the layout stack and return the :code:`in` token. As we'll see in the next
section, as long as the lexer keeps track of its own context (i.e. the stack),
the parser will cope just fine without the virtual end-brace.

Parsing Lonely Braces
*********************

When viewed in the abstract, parsing and tokenising are near-identical tasks yet
the two are very often decomposed into discrete systems with very different
implementations. Lexers operate on streams of text and tokens, while parsers
are typically far less linear, using a parse stack or recursing top-down. A
big reason for this separation is state management: the parser aims to be as
context-free as possible, while the lexer tends to burden the necessary
statefulness. Still, the nature of a stream-oriented lexer makes backtracking
difficult and quite inelegant.

However, simply declaring a parse error to be not an error at all
counterintuitively proves to be an elegant solution our layout problem which
minimises backtracking and state in both the lexer and the parser. Consider the
following definitions found in rlp's BNF:

.. productionlist:: rlp
   VOpen   : `vopen`
   VClose  : `vclose` | `error`

A parse error is recovered and treated as a closing brace. Another point of note
in the BNF is the difference between virtual and non-virtual braces (TODO: i
don't like that the BNF is formatted without newlines :/):

.. productionlist:: rlp
   LetExpr : `let` VOpen Bindings VClose `in` Expr | `let` `{` Bindings `}` `in` Expr

This ensures that non-virtual braces are closed explicitly.

This set of rules is adequete enough to satisfy our basic concerns about line
continations and layout lists. For a more pedantic description of the layout
system, see `chapter 10
<https://www.haskell.org/onlinereport/haskell2010/haskellch10.html>`_ of the
2010 Haskell Report, which I heavily referenced here.

References
----------

* `Python's lexical analysis
  <https://docs.python.org/3/reference/lexical_analysis.html>`_

* `Haskell syntax reference
  <https://www.haskell.org/onlinereport/haskell2010/haskellch10.html>`_
