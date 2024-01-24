rl' Inference Rules
===================

.. rubric::
   [Var]

.. math::
   \frac{x : \tau \in \Gamma}
        {\Gamma \vdash x : \tau}

.. rubric::
   [App]

.. math::
   \frac{\Gamma \vdash f : \alpha \to \beta \qquad \Gamma \vdash x : \alpha}
        {\Gamma \vdash f x : \beta}

