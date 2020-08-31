alphaKanren
===========

WEB -- Revised 31 August 02020.

In the alphaKanren on GitHub,

(fresh (a) (exist (x) (hash x a) (== a x)))

and

(fresh (a) (exist (x) (== a x) (hash x a)))

have identical behavior--both signal errors.

I have changed the alphaKanren code so that `hash` projects its
first argument, making (fresh (a) (exist (x) (== a x) (hash x a)))
legal but (fresh (a) (exist (x) (hash x a) (== a x))) illegal.
This is inherently unrelational.  Is there a way to fix this
behavior?

`tie` does not project its first argument.  The first argument to
`tie` must be a nom/atom, not a unification variable bound to a
nom/atom.

-------

Improved version of the nominal logic programming extensions to miniKanren, based on alphaProlog.
Importantly, this version signals an error if the first argument to hash or tie is not a ground nom (atom).

Original code is described in:

William E. Byrd and Daniel P. Friedman
alphaKanren: A Fresh Name in Nominal Logic Programming
In Proceedings of the 2007 Workshop on Scheme and Functional Programming,
Universite Laval Technical Report DIUL-RT-0701, pp. 79-90

Revised version of the paper:

http://webyrd.net/alphamk/alphamk.pdf
