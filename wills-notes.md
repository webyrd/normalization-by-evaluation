# Will's Notes

* Is it possible to avoid having to write/use `uneval` in miniKanren, by instead runnign `evalo` backwards?  Would this sidestep the fresh names issues?

* Seems like we still need to reason about alpha-equivalence of lambda terms, even with `uneval`.  Try implementing `N.scm` in both alpha-Kanren, and in lambda-Kanren.

* Is there a way to side-step the need for nominal unification or higher-order unification, and still get the proper equality behavior?

* Is my definition of `fresho` correct in the miniKanren version of `N.scm`?  Even if `fresho` is correct, that isn't sufficient to deal with alpha-equivalence.  Could try De Bruijn notation, as Edward suggested.