# normalization-by-evaluation

Relational normalization-by-evaluation (nbe) in miniKanren/alphaKanren.

The Scheme, miniKanren, and alphaKanren code is based on Edward Kmett's (non-relational) Haskell code, presented to Will Byrd during a mini-tutorial on nbe.

This repository includes the code from Michael Ballyntyne's 'faster-miniKanren' repo--please see the 'faster-miniKanren/LICENSE' file for the license for that code.  For all other code in this repository, please see the 'LICENSE' file in the top-level directory of this repository.

Thank you to Michael Arntzenius for a previous tutorial on normalization-by-evaluation, and a separate attempt at implementing normalization-by-evaluation relationally.

Code in this repository:

##### `original-edward-kmett-code`
###### `N.hs` (nbe for untyped call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
###### `N1.hs` (nbe for typed call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
##### `scheme-version`
###### `nbe.scm` (Will Byrd's translation of `N.hs` into Scheme, using the `pmatch` pattern matcher; tested under Chez Scheme)
###### `pmatch.scm` (Oleg Kiselyov's simple pattern matcher, implemented using continuation-passing macros)
##### `miniKanren-version`
###### `nve.scm` (Will Byrd's translation of `nbe.scm` into miniKanren; tested under Chez Scheme)
###### `faster-miniKanren` (Michael Ballantyne's implementation of [faster-miniKanren](https://github.com/michaelballantyne/faster-miniKanren))
##### `alphaKanren-version`
###### `nbe.scm` (Will Byrd's translation of miniKanren `nbe.scm` into alphaKanren; tested under Chez Scheme)
###### `nbe-tests.scm` (Will Byrd's tests for the alphaKanren version of `nbe.scm`; tested under Chez Scheme)
###### `alphaKanren` (Dan Friedman and Will Byrd's implementation of [alphaKanren](https://github.com/webyrd/alphaKanren))
##### `scheme-helpers` (helper code that might be useful in multiple Scheme-related directories)
###### `test-macro.scm` (Simple test macro, adapted from Oleg Kislyov's test macro used in the original Kanren)

Useful tutorials on normalization-by-evaluation:

['Checking Dependent Types with Normalization by Evaluation: A Tutorial' by David Thrane Christiansen](http://www.davidchristiansen.dk/tutorials/nbe/)

[Normalization by Evaluation - David Christiansen (PL Wonks)](https://www.youtube.com/watch?v=CpADWJa-f28)
