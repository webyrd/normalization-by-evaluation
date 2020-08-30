(load "N.scm")
(load "../scheme-helpers/test-macro.scm")

;; WEB comment:
;;
;; The current implementation of alphaKanren uses *restricted nominal unification*,
;; as described on p. 35 of "Nominal Logic Programming" by Cheney and Urban
;; (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.342.6780)
;;
;; As a result, the first argument to a `hash` constraint *must* be a "nom" ("atom").
;; The first argument to a `hash` constraint *must not* be a "normal" unification
;; variable, even if that unification variable has been unified with a nom.
;;
;; For example,
;;
;; (fresh (a) (exist (x) (== a x) (hash x a)))
;;
;; is illegal and signals an error, since the first argument to `hash` is not a nom, even though
;;
;; (fresh (a) (exist (x) (== a x) (hash a a)))
;;
;; is legal.
;;
;; In other words, unifying the unification variable `x` with the nom `a`, does *not* allow us
;; to treat `x` as if it were `a`.
;;
;; This is a super annoying restriction, and feels very non-miniKanreny.
;;
;; As a consolation, at least it is the case that
;;
;; (fresh (a) (exist (x) (hash x a) (== a x)))
;;
;; and
;;
;; (fresh (a) (exist (x) (== a x) (hash x a)))
;;
;; have identical behavior--both signal errors.
;;
;; Has there been any more progress in the nominal unification
;; literature in lifting this restriction?
;;
;; Perhaps we could modify the alphaKanren implementation to allow the
;; constraints requiring the first argument to be a nom to be
;; *delayed*.  These delayed goals would suspend their evaluation
;; until their first argument became ground to a nom.  An error would
;; be signalled if the `run` expression finished execution without all
;; of the delayed goals being evaluated.


(test "simple-nominal-1"
  (run* (q)
    (fresh (a)
      (exist (x)
        (== a x)
        (== q x))))
  '(a.0))

(test "simple-nominal-2"
  (run* (q)
    (fresh (a)
      (== a q)))
  '(a.0))

(test "simple-nominal-3"
  (run* (q)
    (fresh (a)
      (hash a q)))
  '(_.0))

(test "lookupo-1"
  (run* (q)
    (fresh (a b c)
      (exist (env)
        (== `((,a . 1) (,b . 2) (,c . 3)) env)
        (lookupo b env q))))
  '(2))

(test "lookupo-2"
  (run* (q)
    (fresh (a b c)
      (exist (env)
        (== `((,a . 1) (,b . 2) (,c . 3) (,b . ,4)) env)
        (lookupo b env q))))
  '(2))

(test "lookupo-3"
  (run* (q)
    (fresh (a b c d)
      (exist (env)
        (== `((,a . 1) (,b . 2) (,c . 3)) env)
        (lookupo d env q))))
  '())

(test "lookupo-4"
  (run 3 (env)
    (fresh (z)
      (lookupo z env 1)))
  '(((a.0 . 1) . _.0)
    (((_.0 . _.1) (a.0 . 1) . _.2) : ((a.0 . _.0)))
    (((_.0 . _.1) (_.2 . _.3) (a.0 . 1) . _.4) : ((a.0 . _.0) (a.0 . _.2)))))

(test "lookupo-5"
  (run 3 (q)
    (exist (env val)
      (== (list env val) q)
      (fresh (z)
        (lookupo z env val))))
  '((((a.0 . _.0) . _.1) _.0)
    ((((_.0 . _.1) (a.0 . _.2) . _.3) _.2) : ((a.0 . _.0)))
    ((((_.0 . _.1) (_.2 . _.3) (a.0 . _.4) . _.5) _.4) : ((a.0 . _.0) (a.0 . _.2)))))
