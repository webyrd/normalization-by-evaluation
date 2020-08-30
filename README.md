# normalization-by-evaluation

Normalization-by-evaluation in miniKanren

Based on a recent conversation with Edward Kmett, and Edward's (non-relational) Haskell code.

Thank you to Michael Arntzenius for a previous tutorial on normalization-by-evaluation, and a separate attempt at implementing normalization-by-evaluation relationally.

Code in this repository:

* `original-edward-kmett-code`
** `N.hs` (nbe for untyped call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
** `N1.hs` (nbe for typed call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
* `scheme-version`
** `N.scm` (Will Byrd's translation of `N.hs` into Scheme, using the `pmatch` pattern matcher; tested under Chez Scheme)
** `pmatch.scm` (Oleg Kiselyov's simple pattern matcher, implemented using continuation-passing macros)
* `miniKanren-version`
** `N.scm` (Will Byrd's translation of `N.scm` into miniKanren; tested under Chez Scheme)
** `faster-miniKanren` (Michael Ballantyne's implementation of [faster-miniKanren](https://github.com/michaelballantyne/faster-miniKanren))

Useful tutorials on normalization-by-evaluation:

['Checking Dependent Types with Normalization by Evaluation: A Tutorial' by David Thrane Christiansen](http://www.davidchristiansen.dk/tutorials/nbe/)

[Normalization by Evaluation - David Christiansen (PL Wonks)](https://www.youtube.com/watch?v=CpADWJa-f28)
