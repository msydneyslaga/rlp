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

*************
The G-Machine
*************

.. literalinclude:: /../../src/GM.hs
   :dedent:
   :start-after: -- >> [ref/Instr]
   :end-before: -- << [ref/Instr]
   :caption: src/GM.hs

