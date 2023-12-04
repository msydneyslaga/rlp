================================
G-Machine State Transition Rules
================================

*********************
Core Transition Rules
*********************

#. Lookup a global by name and push its value onto the stack

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

#. Allocate an int node on the heap, and push the address of the newly created
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

#. Allocate an application node on the heap, applying the top of the stack to
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

#. Push a function's argument onto the stack

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

#. Tidy up the stack after instantiating a supercombinator

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

#. If the top of the stack is in WHNF (currently this just means a number) is on
   top of the stack, :code:`Unwind` considers evaluation complete. In the case
   where the dump is **not** empty, the instruction queue and stack is restored
   from the top.

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

#. Bulding on the previous rule, in the case where the dump **is** empty, leave
   the machine in a halt state (i.e. with an empty instruction queue).

   .. math::
      \gmrule
      { \mathtt{Unwind} : \nillist
      & a : s
      & \nillist
      & h
      \begin{bmatrix}
           a : \mathtt{NNum} \; n
      \end{bmatrix}
      & m
      }
      { \nillist
      & a : s
      & \nillist
      & h
      & m
      }

#. If an application is on top of the stack, :code:`Unwind` continues unwinding

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

#. When a supercombinator is on top of the stack (and the correct number of
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

#. Pop the stack, and update the nth node to point to the popped address

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

#. Pop the stack.

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

#. Follow indirections while unwinding

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

#. Allocate uninitialised heap space

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

#. Evaluate the top of the stack to WHNF

   .. math::
      \gmrule
      { \mathtt{Eval} : i
      & a : s
      & d
      & h
      & m
      }
      { \mathtt{Unwind} : \nillist
      & a : \nillist
      & \langle i, s \rangle : d
      & h
      & m
      }

#. Reduce a primitive binary operator :math:`*`.

   .. math::
      \gmrule
      { * : i
      & a_1 : a_2 : s
      & d
      & h
      \begin{bmatrix}
            a_1 : x \\
            a_2 : y
      \end{bmatrix}
      & m
      }
      { i
      & a' : s
      & d
      & h
      \begin{bmatrix}
           a' : (x * y)
      \end{bmatrix}
      & m
      }

#. Reduce a primitive unary operator :math:`\neg`.

   .. math::
      \gmrule
      { \neg : i
      & a : s
      & d
      & h
      \begin{bmatrix}
            a : x
      \end{bmatrix}
      & m
      }
      { i
      & a' : s
      & d
      & h
      \begin{bmatrix}
           a' : (\neg x)
      \end{bmatrix}
      & m
      }
   
***************
Extension Rules
***************

#. A sneaky trick to enable sharing of :code:`NNum` nodes. We note that the
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

#. In order for the previous rule to be effective, we are also required to take
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

