# Configuration file for the Sphinx documentation builder.
#
# For the full list of built-in configuration values, see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Project information -----------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#project-information

project = "rl'"
copyright = '2023, madeleine sydney ślaga'
author = 'madeleine sydney slaga'

# -- General configuration ---------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#general-configuration

extensions = ['sphinx.ext.imgmath']

# templates_path = ['_templates']
exclude_patterns = []

# for the haskell girlies
highlight_language = 'haskell'
add_function_parentheses = False

# -- Options for HTML output -------------------------------------------------
# https://www.sphinx-doc.org/en/master/usage/configuration.html#options-for-html-output

html_theme = 'alabaster'
# html_static_path = ['_static']

# -- Options for LaTeX image math --------------------------------------------
imgmath_latex_preamble = r'''
\usepackage{amsmath}
\usepackage{tabularray}

\newcommand{\transrule}[2]
    {\begin{tblr}{|rrrlc|}
         \hline
         & #1 \\
         \implies & #2 \\
         \hline
    \end{tblr} }

\newcommand{\gmrule}[2]
    {\begin{tblr}{|rrrrll|}
         \hline
         & #1 \\
         \implies & #2 \\
         \hline
    \end{tblr} }

\newcommand{\nillist}{[\,]}

\newcommand{\concat}{+\kern-1.3ex+\kern0.8ex}
'''

imgmath_image_format = 'svg'
imgmath_font_size = 14

# helps with inlining:
# https://www.sphinx-doc.org/en/master/usage/extensions/math.html#confval-imgmath_use_preview
imgmath_use_preview = True

