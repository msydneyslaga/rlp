The *Spineless Tagless G-Machine*
=================================

WIP. This will hopefully be expanded into a thorough walkthrough of the state
machine.

Evaluation is complete when a single :code:`NNum` remains on the stack and the
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

Evaluate the argument of a unary operation with internal :code:`Prim`
constructor :code:`O`.

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
        { x : \nillist
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

Perform a binary operation :math:`o(x,y)` associated with internal :code:`Prim`
constructor :code:`O` on two :code:`NNum` s both in normal form.

.. math::
   \transrule
   { f : a_1 : a_2 : s
   & d
   & h
   \begin{bmatrix}
        f : \mathtt{NPrim} \; \mathtt{O} \\
        a_1 : \mathtt{NAp} \; f \; (\mathtt{NNum} \; x) \\
        a_2 : \mathtt{NAp} \; a_1 \; (\mathtt{NNum} \; y)
   \end{bmatrix}
   & g
   }
   { a_2 : s
   & d
   & h
   \begin{bmatrix}
        a_2 : \mathtt{NNum} \; (o(x,y))
   \end{bmatrix}
   & g
   }

In a conditional primitive, perform the reduction if the condition has been
evaluated as True (:code:`NData 1 []`).

.. math::
   \transrule
   { f : a_1 : a_2 : a_3 : s
   & d
   & h
   \begin{bmatrix}
        f : \mathtt{NPrim} \; \mathtt{IfP} \\
        c : \mathtt{NPrim} \; (\mathtt{NData} \; 1 \; []) \\
        a_1 : \mathtt{NAp} \; f \; c \\
        a_2 : \mathtt{NAp} \; a_1 \; x \\
        a_3 : \mathtt{NAp} \; a_2 \; y
   \end{bmatrix}
   & g
   }
   { x : s
   & d
   & h
   & g
   }

In a conditional primitive, perform the reduction if the condition has been
evaluated as False (:code:`NData 0 []`).

.. math::
   \transrule
   { f : a_1 : a_2 : a_3 : s
   & d
   & h
   \begin{bmatrix}
        f : \mathtt{NPrim} \; \mathtt{IfP} \\
        c : \mathtt{NPrim} \; (\mathtt{NData} \; 0 \; []) \\
        a_1 : \mathtt{NAp} \; f \; c \\
        a_2 : \mathtt{NAp} \; a_1 \; x \\
        a_3 : \mathtt{NAp} \; a_2 \; y
   \end{bmatrix}
   & g
   }
   { y : s
   & d
   & h
   & g
   }


In a conditional primitive, evaluate the condition.

.. math::
   \transrule
   { f : a_1 : \nillist
   & d
   & h
   \begin{bmatrix}
        f : \mathtt{NPrim} \; \mathtt{IfP} \\
        a_1 : \mathtt{NAp} \; f \; x
   \end{bmatrix}
   & g
   }
   { x : \nillist
   & (f : a_1 : \nillist) : d
   & h
   & g
   }

Construct :code:`NData` out of a constructor and its arguments

.. math::
   \transrule
   { c : a_1 : \ldots : a_n : \nillist
   & d
   & h
   \begin{bmatrix}
        c : \mathtt{NPrim} \; (\mathtt{ConP} \; t \; n) \\
        a_1 : \mathtt{NAp} \; c \; x_1 \\
        \vdots \\
        a_n : \mathtt{NAp} \; a_{n-1} \; x_n
   \end{bmatrix}
   & g
   }
   { a_n : \nillist
   & d
   & h
   \begin{bmatrix}
        a_n : \mathtt{NData} \; t \; [x_1, \ldots, x_n]
   \end{bmatrix}
   & g
   }
