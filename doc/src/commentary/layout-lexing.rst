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

But What About Haskell?
***********************

Parsing Haskell -- and thus rl' -- is only slightly more complex than Python,
but the design is certainly more sensitive. 

.. code-block:: haskell

   -- line folds
   something = this is a
       single expression

   -- an extremely common style found in haskell
   data Some = Data
       { is    :: Presented
       , in    :: This
       , silly :: Style
       }

   -- another style oddity
   -- note that this is not a single
   -- continued line! `look at`,
   -- `this odd`, and `alignment` are all
   -- discrete items!
   anotherThing = do look at
                     this odd
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

