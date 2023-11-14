The *Spineless Tagless G-Machine*
=================================

WIP. This will hopefully be expanded into a thorough explanation of the state
machine.

Evaluation is complete when a single \texttt{NNum} remains on the stack and the
dump is empty.

.. math::
   \transrule
   { a : \nillist
   & \nillist
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & g
   }
   { \mathtt{HALT}
   }

Dereference an indirection passed as an argument to a function.

.. math::
   \transrule
        {a : s & d & h
           \begin{bmatrix}
               a : \mathtt{NAp} \; a_1 \; a_2 \\
               a_2 : \mathtt{NInd} \; a_3
           \end{bmatrix} & g}
        {a : s & d & h[a : \mathtt{NAp} \; a_1 \; a_3] & g}

Dereference an indirection on top of the stack.

.. math::
   \transrule
        {p : s & d & h
            \begin{bmatrix}
                p : \mathtt{NInd} \; a
            \end{bmatrix} & g}
        {a : s & d & h & g}

Perform a unary operation :math:`o(n)` with internal :code:`Prim` constructor
:code:`O` on an argument in normal form.

.. math::
   \transrule
        { f : a : s
        & d
        & h
        \begin{bmatrix}
            f : \mathtt{NPrim} \; \mathtt{O} \\
            a : \mathtt{NAp} \; f \; x \\
            x : \mathtt{NNum} \; n
        \end{bmatrix}
        & g
        }
        { a : s
        & d
        & h
        \begin{bmatrix}
            a : \mathtt{NNum} \; (o(n))
        \end{bmatrix}
        & g
        }

Perform a unary operation :math:`o(n)` with internal :code:`Prim` constructor
:code:`O` on an unevaluated argument.

.. math::
   \transrule
        { f : a : \nillist
        & d
        & h
        \begin{bmatrix}
            f : \mathtt{NPrim} \; \mathtt{O} \\
            a : \mathtt{NAp} \; f \; x
        \end{bmatrix}
        & g
        }
        { b : \nillist
        & (f : a : \nillist) : d
        & h
        & g
        }

Restore the stack when a sub-computation has completed.

.. math::
   \transrule
   { a : \nillist
   & s : d
   & h
   \begin{bmatrix}
        a : \mathtt{NNum} \; n
   \end{bmatrix}
   & g
   }
   { s
   & d
   & h
   & g
   }

Reduce a supercombinator and update the root with the :math:`\beta`-reduced form

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

