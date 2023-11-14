The *Spineless Tagless G-Machine*
=================================

WIP. Here's a typeset state transition rule describing the action of
dereferencing indirections when passed as function arguments.

.. math::
   \transrule
        {a : s & d & h
           \begin{bmatrix}
               a : \mathtt{NAp} \; a_1 \; a_2 \\
               a_2 : \mathtt{NInd} \; a_3
           \end{bmatrix} & f}
        {a : s & d & h[a : \mathtt{NAp} \; a_1 \; a_3] & f}
   :label: rule1

