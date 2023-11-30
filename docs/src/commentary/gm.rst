The *G-Machine*
===============

**********
Motivation
**********

Our initial model, the *Template Instantiation Machine* (TIM) was a very
straightforward solution to compilation, but its core design has a major
Achilles' heel, being that Compilation is interleaved with evaluation -- The
heap nodes for supercombinators hold uninstantiated expressions, i.e. raw ASTs
straight from the parser. When a supercombinator is found on the stack during
evaluation, the template expression is instantiated (compiled) on the spot.

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

The process of instantiating a supercombinator goes something like this

1. Augment the environment with bindings to the arguments.

2. Using the local augmented environment, instantiate the supercombinator body
   on the heap.

3. Remove the nodes applying the supercombinator to its arguments from the
   stack.

4. Push the address to the newly instantiated body onto the stack.

.. literalinclude:: /../../src/TIM.hs
   :dedent:
   :start-after: -- >> [ref/scStep]
   :end-before: -- << [ref/scStep]
   :caption: src/TIM.hs

Instantiating the supercombinator's body in this way is the root of our
Achilles' heel. Traversing a tree structure is a very non-linear task unfit for
an assembly target. The goal of our new G-Machine is to compile a *linear
sequence of instructions* which instantiate the expression at execution.

**************************
Trees and Vines, in Theory
**************************

WIP. state transition rules

1. Lookup a global by name and push its value onto the stack

.. math::
   \gmrule
   { \mathtt{PushGlobal} \; f : i
   & s
   & h
   & m
   \begin{bmatrix}
        f : a
   \end{bmatrix}
   }
   { i
   & a : s
   & h
   & m
   }

2. Allocate an int node on the heap, and push the address of the newly created
   node onto the stack

.. math::
   \gmrule
   { \mathtt{PushInt} \; n : i
   & s
   & h
   & m
   }
   { i
   & a : s
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & m
   }

3. Allocate an application node on the heap, applying the top of the stack to
   the address directly below it. The address of the application node is pushed
   onto the stack.

.. math::
   \gmrule
   { \mathtt{MkAp} : i
   & f : x : s
   & h
   & m
   }
   { i
   & a : s
   & h
   \begin{bmatrix}
        a : \mathtt{NAp} \; f \; x
   \end{bmatrix}
   & m
   }

4. Push a function's argument onto the stack

.. math::
   \gmrule
   { \mathtt{Push} \; n : i
   & a_0 : \ldots : a_{n+1} : s
   & h
   \begin{bmatrix}
        a_{n+1} : \mathtt{NAp} \; a_n \; a'_n
   \end{bmatrix}
   & m
   }
   { i
   & a'_n : a_0 : \ldots : a_{n+1} : s
   & h
   & m
   }

5. Tidy up the stack after instantiating a supercombinator

.. math::
   \gmrule
   { \mathtt{Slide} \; n : i
   & a_0 : \ldots : a_n : s
   & h
   & m
   }
   { i
   & a_0 : s
   & h
   & m
   }

6. If a number is on top of the stack, :code:`Unwind` leaves the machine in a
   halt state

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & m
   }
   { \nillist
   & a : s
   & h
   & m
   }

7. If an application is on top of the stack, :code:`Unwind` continues unwinding

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & h
   \begin{bmatrix}
        a : \mathtt{NAp} \; f \; x
   \end{bmatrix}
   & m
   }
   { \mathtt{Unwind} : \nillist
   & f : a : s
   & h
   & m
   }

8. When a global node is on top of the stack (and the correct number of
   arguments have been provided), :code:`Unwind` jumps to the supercombinator's
   code (:math:`\beta`-reduction)

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a_0 : \ldots : a_n : s
   & h
   \begin{bmatrix}
        a_0 : \mathtt{NGlobal} \; n \; c
   \end{bmatrix}
   & m
   }
   { c
   & a_0 : \ldots : a_n : s
   & h
   & m
   }

**************************
Evaluation: Slurping Vines
**************************

WIP.

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

