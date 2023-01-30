parse.hs
========

A Haskell program that will take a string and attempt to parse it as a formula of first order logic.  If it can parse it, it will return the formula, its main operator, the subformula(s) it is operating on, and the full parse tree. The purpose of this is just for me to learn a bit of Haskell.

Grammar
-------

*Names:* a, b, ... t  
*Variables:* u, v, ... z
*Predicates:* A, B, ... Z

*Atoms:* if P is a predicate letter and x...y are zero or more terms, then Px...y is an atomic formula.

*Compounds:* if X and Y are formulas and z is a variable, then ~X, (X&Y), (XvY), (X>Y), (X$Y), AzX and EzX are formulas.
