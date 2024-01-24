The *G-Machine*
===============

The G-Machine (graph machine) is the current heart of rlpc, until we potentially
move onto a STG (spineless tagless graph machine) or a TIM (three-instruction
machine). rl' source code is desugared into Core; a dumbed-down subset of rl',
and then compiled to G-Machine code, which is then finally translated to the
desired target.

**********
Motivation
**********

Our initial model, the *Template Instantiator* (TI) was a very straightforward
solution to compilation, but its core design has a major Achilles' heel, being
that compilation is interleaved with evaluation -- The heap nodes for
supercombinators hold uninstantiated expressions, i.e. raw ASTs straight from
the parser. When a supercombinator is found on the stack during evaluation, the
template expression is instantiated (compiled) on the spot. This makes
translation to an assembly difficult, undermining the point of an intermediate
language.

.. math::
   \transrule
   { a_0 : a_1 : \ldots : a_n : s
   & d
   & h
   \begin{bmatrix}
        a_0 : \mathtt{NSupercomb} \; [x_1,\ldots,x_n] \; e
   \end{bmatrix}
   & g
   }
   { a_n : s
   & d
   & h'
   & g
   \\
   & \SetCell[c=3]{c}
   \text{where } h' = \mathtt{instantiateU} \; e \; a_n \; h \; g
   }

The process of instantiating a supercombinator goes something like this:

1. Augment the environment with bindings to the arguments.

2. Using the local augmented environment, instantiate the supercombinator body
   on the heap.

3. Remove the nodes applying the supercombinator to its arguments from the
   stack.

4. Push the address to the newly instantiated body onto the stack.

.. literalinclude:: /../../src/TI.hs
   :dedent:
   :start-after: -- >> [ref/scStep]
   :end-before: -- << [ref/scStep]
   :caption: src/TI.hs

Instantiating the supercombinator's body in this way is the root of our
Achilles' heel. Traversing a tree structure is a very non-linear task unfit for
an assembly target. The goal of our new G-Machine is to compile a *linear
sequence of instructions* which, **when executed**, build up a graph
representing the code.

**************************
Trees and Vines, in Theory
**************************

Rather than instantiating an expression at runtime -- traversing the AST and
building a graph -- we want to compile all expressions at compile-time,
generating a linear sequence of instructions which may be executed to build the
graph.

**************************
Evaluation: Slurping Vines
**************************

WIP.

Laziness
--------

WIP.

* Instead of :code:`Slide (n+1); Unwind`, do :code:`Update n; Pop n; Unwind`

****************************
Compilation: Squashing Trees
****************************

WIP.

Notice that we do not keep a (local) environment at run-time. The environment
only exists at compile-time to map local names to stack indices. When compiling
a supercombinator, the arguments are enumerated from zero (the top of the
stack), and passed to :code:`compileR` as an environment.

.. literalinclude:: /../../src/GM.hs
   :dedent:
   :start-after: -- >> [ref/compileSc]
   :end-before: -- << [ref/compileSc]
   :caption: src/GM.hs

Of course, variables being indexed relative to the top of the stack means that
they will become inaccurate the moment we push or pop the stack a single time.
The way around this is quite simple: simply offset the stack when w

.. literalinclude:: /../../src/GM.hs
   :dedent:
   :start-after: -- >> [ref/compileC]
   :end-before: -- << [ref/compileC]
   :caption: src/GM.hs

