================================
G-Machine State Transition Rules
================================

*********************
Core Transition Rules
*********************

1. Lookup a global by name and push its value onto the stack

.. math::
   \gmrule
   { \mathtt{PushGlobal} \; f : i
   & s
   & d
   & h
   & m
   \begin{bmatrix}
        f : a
   \end{bmatrix}
   }
   { i
   & a : s
   & d
   & h
   & m
   }

2. Allocate an int node on the heap, and push the address of the newly created
   node onto the stack

.. math::
   \gmrule
   { \mathtt{PushInt} \; n : i
   & s
   & d
   & h
   & m
   }
   { i
   & a : s
   & d
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
   & d
   & h
   & m
   }
   { i
   & a : s
   & d
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
   & a_0 : \ldots : a_n : s
   & d
   & h
   & m
   }
   { i
   & a_n : a_0 : \ldots : a_n : s
   & d
   & h
   & m
   }

5. Tidy up the stack after instantiating a supercombinator

.. math::
   \gmrule
   { \mathtt{Slide} \; n : i
   & a_0 : \ldots : a_n : s
   & d
   & h
   & m
   }
   { i
   & a_0 : s
   & d
   & h
   & m
   }

6. If a number is on top of the stack, :code:`Unwind` leaves the machine in a
   halt state

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & d
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & m
   }
   { \nillist
   & a : s
   & d
   & h
   & m
   }

7. If an application is on top of the stack, :code:`Unwind` continues unwinding

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & d
   & h
   \begin{bmatrix}
        a : \mathtt{NAp} \; f \; x
   \end{bmatrix}
   & m
   }
   { \mathtt{Unwind} : \nillist
   & f : a : s
   & d
   & h
   & m
   }

8. When a supercombinator is on top of the stack (and the correct number of
   arguments have been provided), :code:`Unwind` sets up the stack and jumps to
   the supercombinator's code (:math:`\beta`-reduction)

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a_0 : \ldots : a_n : s
   & d
   & h
   \begin{bmatrix}
        a_0 : \mathtt{NGlobal} \; n \; c \\
        a_1 : \mathtt{NAp} \; a_0 \; e_1 \\
        \vdots \\
        a_n : \mathtt{NAp} \; a_{n-1} \; e_n \\
   \end{bmatrix}
   & m
   }
   { c
   & e_1 : \ldots : e_n : a_n : s
   & d
   & h
   & m
   }

9. Pop the stack, and update the nth node to point to the popped address

.. math::
   \gmrule
   { \mathtt{Update} \; n : i
   & e : f : a_1 : \ldots : a_n : s
   & d
   & h
   \begin{bmatrix}
        a_1 : \mathtt{NAp} \; f \; e \\
        \vdots \\
        a_n : \mathtt{NAp} \; a_{n-1} \; e_n
   \end{bmatrix}
   & m
   }
   { i
   & f : a_1 : \ldots : a_n : s
   & d
   & h
   \begin{bmatrix}
        a_n : \mathtt{NInd} \; e
   \end{bmatrix}
   & m
   }

10. Pop the stack.

.. math::
   \gmrule
   { \mathtt{Pop} \; n : i
   & a_1 : \ldots : a_n : s
   & d
   & h
   & m
   }
   { i
   & s
   & d
   & h
   & m
   }

11. Follow indirections while unwinding

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & d
   & h
   \begin{bmatrix}
        a : \mathtt{NInd} \; a'
   \end{bmatrix}
   & m
   }
   { \mathtt{Unwind} : \nillist
   & a' : s
   & d
   & h
   & m
   }

12. Allocate uninitialised heap space

.. math::
   \gmrule
   { \mathtt{Alloc} \; n : i
   & s
   & d
   & h
   & m
   }
   { i
   & a_1 : \ldots : a_n : s
   & d
   & h
   \begin{bmatrix}
        a_1 : \mathtt{NUninitialised} \\
        \vdots \\
        a_n : \mathtt{NUninitialised} \\
   \end{bmatrix}
   & m
   }

13. When unwinding, if the top of the stack is in WHNF (currently this just
    means a number), pop the dump

.. math::
   \gmrule
   { \mathtt{Unwind} : \nillist
   & a : s
   & \langle i', s' \rangle : d
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & m
   }
   { i'
   & a : s'
   & d
   & h
   & m
   }

14. Evaluate the top of the stack to WHNF

.. math::
   \gmrule
   { \mathtt{Eval} : i
   & a : s
   & d
   & h
   & m
   }
   { i
   & a : \nillist
   & \langle i, s \rangle
   & h
   & m
   }

***************
Extension Rules
***************

1. A sneaky trick to enable sharing of :code:`NNum` nodes. We note that the
   global environment is a mapping of plain old strings to heap addresses.
   Strings of digits are not considered valid identifiers, so putting them on
   the global environment will never conflict with a supercombinator! We abuse
   this by modifying Core Rule 2 to update the global environment with the new
   node's address. Consider how this rule might impact garbage collection
   (remember that the environment is intended for *globals*).

.. math::
   \gmrule
   { \mathtt{PushInt} \; n : i
   & s
   & d
   & h
   & m
   }
   { i
   & a : s
   & d
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & m
   \begin{bmatrix}
        n' : a
   \end{bmatrix}
   \\
   \SetCell[c=5]{c}
   \text{where $n'$ is the base-10 string rep. of $n$}
   }

2. In order for Extension Rule 1. to be effective, we are also required to take
   action when a number already exists in the environment:

.. math::
   \gmrule
   { \mathtt{PushInt} \; n : i
   & s
   & d
   & h
   & m
   \begin{bmatrix}
        n' : a
   \end{bmatrix}
   }
   { i
   & a : s
   & d
   & h
   & m
   \\
   \SetCell[c=5]{c}
   \text{where $n'$ is the base-10 string rep. of $n$}
   }

