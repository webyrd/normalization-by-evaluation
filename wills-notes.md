# Will's Notes

* Is it possible to avoid having to write/use `uneval` in miniKanren, by instead runnign `evalo` backwards?  Would this sidestep the fresh names issues?

* Seems like we still need to reason about alpha-equivalence of lambda terms, even with `uneval`.  Try implementing `N.scm` in both alpha-Kanren, and in lambda-Kanren.

* Is there a way to side-step the need for nominal unification or higher-order unification, and still get the proper equality behavior?

* Is my definition of `fresho` correct in the miniKanren version of `N.scm`?  Even if `fresho` is correct, that isn't sufficient to deal with alpha-equivalence.  Could try De Bruijn notation, as Edward suggested.

* My comment in `alphaKanren/N-tests.scm`:

WEB (fresh (a) (exist (x) (== a x) (hash x a))) signals an error,
since the first arg to `hash` is a nom, even though (fresh (a)
(exist (x) (== a x) (hash a a))) is legal.

This feels overly restrictive.  Once we unify `x` and `a`, shouldn't
uses of `x` behave like uses of `a`?  Of course,
(fresh (a) (exist (x) (hash x a) (== a x))) should work the same as
(fresh (a) (exist (x) (== a x) (hash x a))).  How does alphaProlog
behave in this case?
