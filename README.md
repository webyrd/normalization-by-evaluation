# normalization-by-evaluation

Relational normalization-by-evaluation (nbe) in miniKanren/alphaKanren.

The Scheme, miniKanren, and alphaKanren code is based on Edward Kmett's (non-relational) Haskell code, presented to Will Byrd during a mini-tutorial on nbe.

Edward live codes a normalization-by-evaluation system in this video [YOW! Lambda Jam 2020 - Edward Kmett - Cadenza Building Fast Functional Languages Fast](https://www.youtube.com/watch?v=25RmUl88jSw), starting at 6:30

This repository includes the code from Michael Ballyntyne's 'faster-miniKanren' repo--please see the 'faster-miniKanren/LICENSE' file for the license for that code.  For all other code in this repository, please see the 'LICENSE' file in the top-level directory of this repository.

Thank you to Michael Arntzenius for a previous tutorial on normalization-by-evaluation, and a separate attempt at implementing normalization-by-evaluation relationally.

Thanks to Nada Amin for suggesting improved file names, for adding tests, and for discussing normalization-by-evaluation, and its connections to relational programming.

During a hacking session with Michael Ballantyne, Michael found and fixed a subtle tagging error in the naive miniKanren implementation; we also implemented the `fresh` variable generation used in the naive miniKanren implementation during that session, which seems to side-step the need for nominal logic programming or de Bruijn representation of variables.

Nada Amin asked whether, using this "naive" freshness technique, `(lambda (x) x)` and `(lambda (y) y)` normalize to the name expression.  This led to a conversation about using nbe using the "naive" freshness implementation for nominal-logic style relational programming.


Currently the most interesting relational versions seem to be the "naive" versions, especially `miniKanren-version/naive/nbe-untagged.scm` and `miniKanren-version/naive/nbe-untagged-extended.scm`.

The nominal logic programming version of nbe using alphaKanren does not seem to work fully relationally.  Also, I think the implementation of alphaKanren itself may rely on a subtle use of `eq?` which is sound in R5RS, but whose behavior is undefined in R6RS.  Beware!  If anything, this code is probably most useful for trying to understand possible issues and limitations with relational programming in alphaKanren, since I've never gotten this style of interpreter to work fully relationally.

Also, please be careful with the non-"naive" versions of the relational code in general, since some of these versions are half-finished experiments.  The untagged "naive" versions of the code are the most appealing to me, anyway, and are closest in spirit to the "classic" relational Scheme interpreters in miniKanren.


Code in this repository:

##### `original-edward-kmett-code`
###### `N.hs` (nbe for untyped call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
###### `N1.hs` (nbe for typed call-by-value lambda-calculus in Haskell, from Edward Kmett's mini-tutorial for Will Byrd)
###### `deBruijn`
####### `NB.hs` (nbe for untyped call-by-value lambda-calculus in Haskell, using de Bruijn representation of variables, from Edward Kmett's mini-tutorial for Will Byrd)
####### `NB2.hs` (nbe for typed call-by-value lambda-calculus in Haskell, using de Bruijn representation of variables, from Edward Kmett's mini-tutorial for Will Byrd)
##### `scheme-version` (Scheme version of Edward Kmett's Haskell nbe code)
###### `nbe.scm` (Will Byrd's translation of `N.hs` into Scheme, using the `pmatch` pattern matcher; tested under Chez Scheme)
###### `pmatch.scm` (Oleg Kiselyov's simple pattern matcher, implemented using continuation-passing macros)
##### `miniKanren-version` (miniKanren versions of Edward Kmett's Haskell nbe code)
###### `naive` (relational nbe code)
####### `nbe.scm` (naive relational nbe code, with tagging of expressions and values)
####### `nbe-tests.scm`
####### `nbe-untagged.scm` (naive relational nbe code, with extra tagging removed)
####### `nbe-untagged-tests.scm`
####### `nbe-untagged-extended.scm` (naive relational nbe code, with extra tagging removed, and the language extended)
####### `nbe-untagged-extended-tests.scm`
###### `deBruijn` (relational nbe code using de Bruijn notation for variables)
####### `nbe.scm` (nbe code using de Bruijn notation)
####### `nbe-tests.scm`
####### `nbe-extended.scm` (nbe code using de Bruijn notation, with the language extended)
####### `nbe-extended-tests.scm`
###### `faster-miniKanren` (Michael Ballantyne's implementation of [faster-miniKanren](https://github.com/michaelballantyne/faster-miniKanren))
##### `alphaKanren-version` (*broken* nominal logic programming version of relational nbe code)
###### `nbe.scm` (Will Byrd's translation of miniKanren `nbe.scm` into alphaKanren; tested under Chez Scheme)
###### `nbe-tests.scm` (Will Byrd's tests for the alphaKanren version of `nbe.scm`; tested under Chez Scheme)
###### `alphaKanren` (Dan Friedman and Will Byrd's implementation of [alphaKanren](https://github.com/webyrd/alphaKanren))
##### `scheme-helpers` (helper code that might be useful in multiple Scheme-related directories)
###### `test-macro.scm` (simple test macro, adapted from Oleg Kislyov's test macro used in the original Kanren)
###### `pmatch.scm` (Oleg Kiselyov's simple pattern-matching macro)

Useful tutorials on normalization-by-evaluation:

['Cadenza Building Fast Functional Languages Fast' by Edward Kmett, YOW! Lambda Jam 2020](https://www.youtube.com/watch?v=25RmUl88jSw), starting at 6:30

['Checking Dependent Types with Normalization by Evaluation: A Tutorial' by David Thrane Christiansen](http://www.davidchristiansen.dk/tutorials/nbe/)

[Normalization by Evaluation - David Christiansen (PL Wonks)](https://www.youtube.com/watch?v=CpADWJa-f28)
