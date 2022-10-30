# Will's Notes

```
(((lambda (f)
    ((lambda (x) (f (lambda (v) ((x x) v))))
     (lambda (x) (f (lambda (v) ((x x) v))))))
  (lambda (list?)
    (lambda (l)
      (if (null? l)
          #t
          (if (pair? l)
              (list? (cdr l))
              #f)))))
 '(1 2 3))
=> #t
```

```
> (define Z
    (lambda (f)
      ((lambda (x) (f (lambda (v) ((x x) v))))
       (lambda (x) (f (lambda (v) ((x x) v)))))))
> (define F
    (lambda (list?)
      (lambda (l)
        (if (null? l)
            #t
            (if (pair? l)
                (list? (cdr l))
                #f)))))
> (Z F)
#<procedure>
> (F (Z F))
#<procedure>
> ((Z F) '(1 2 3))
#t
> ((F (Z F)) '(1 2 3))
#t
> ((F (Z F)) '(1 2 3 4 5 6 7))
#t
```

If I uneval the closure resulting from `(Z F)` and from `(F (Z F))`, do I get the same `lambda` expression?

That is, is `(uneval (eval '(Z F)))` equal to `(uneval (eval '(F (Z F))))`

If so, I might be able to synthesize `Z` using the relational `eval` and `uneval`.



The new hotness is the de Bruijn + nbe version!

Things to try:

1) table `evalo` and/or `unevalo`.  Try tabling on normal forms, as well as on expr + env.

2) big-step relational abstract interpretation

3) try implementing small-step reducer using de Bruijn + nbe, if that makes sense

4) try simultaneously doing big-step interp and small-step reduction, with the idea that the small-step reducer can detect loops, find fixpoints, etc.





* Is it possible to avoid having to write/use `uneval` in miniKanren, by instead running `evalo` backwards?  Would this sidestep the fresh names issues? [I don't think so.  `evalo` and `unevalo` do different things.]

* Seems like we still need to reason about alpha-equivalence of lambda terms, even with `uneval`.  Try implementing `nbe.scm` in both alpha-Kanren, and in lambda-Kanren. [done!]

* Is there a way to side-step the need for nominal unification or higher-order unification, and still get the proper equality behavior? [yes!  de Bruijn + nbe!  Thanks, Edward!]

* Is my definition of `fresho` correct in the miniKanren version of `nbe.scm`?  Even if `fresho` is correct, that isn't sufficient to deal with alpha-equivalence.  Could try De Bruijn notation, as Edward suggested.

* See my comment at the top of `alphaKanren/nbe-tests.scm` about the annoying restrictions on the use of unification variables.  Is there a way around these restrictions?  What is the impact of these restrictions?
