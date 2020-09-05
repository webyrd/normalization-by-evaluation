(load "nbe.scm")
(load "../scheme-helpers/test-macro.scm")

;; WEB comment:
;;
;; The current implementation of alphaKanren uses *restricted nominal unification*,
;; as described on p. 35 of "Nominal Logic Programming" by Cheney and Urban
;; (http://citeseerx.ist.psu.edu/viewdoc/summary?doi=10.1.1.342.6780)
;;
;; As a result, the first argument to a `hash` constraint *must* be a
;; ground "nom" ("atom").
;;
;;
;; In the alphaKanren on GitHub,
;;
;; (fresh (a) (exist (x) (hash x a) (== a x)))
;;
;; and
;;
;; (fresh (a) (exist (x) (== a x) (hash x a)))
;;
;; have identical behavior--both signal errors.
;;
;; I have changed the alphaKanren code so that `hash` projects its
;; first argument, making (fresh (a) (exist (x) (== a x) (hash x a)))
;; legal but (fresh (a) (exist (x) (hash x a) (== a x))) illegal.
;; This is inherently unrelational.  Is there a way to fix this
;; behavior?
;;
;; See eval-expro-0h for an example of where this behavior isn't
;; sufficient when running `eval-expro` backwards, resulting in a
;; run-time error.
;;
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

(test "simple-nominal-4"
  (run* (q)
    (exist (e)
      (fresh (a)
        (== `(Var ,a) `(Var ,e)))))
  '(_.0))

(test "simple-nominal-4b"
  (run* (q)
    (fresh (a)
      (exist (e)
        (== `(Var ,a) `(Var ,e)))))
  '(_.0))

(test "simple-nominal-5"
  (run* (q)
    (fresh (a)
      (exist (e)
        (== e a)
        (hash e e))))
  '())

#|
(test "simple-nominal-5b"
  (run* (q)
    (fresh (a)
      (exist (e)
        (hash e e)
        (== e a))))
  '())
;; Exception in hash: first argument is not a nom with irritant (susp-tag () #<procedure at alphaKanren.scm:1915>)
;; Type (debug) to enter the debugger.
|#

(test "simple-nominal-6"
  (run* (q)
    (fresh (a)
      (exist (e)
        (== (tie a e) q)))
    (exist (e)
      (fresh (a)
        (== (tie a e) q))))
  '((tie-tag a.0 _.0)))


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
    (((_.0 . _.1) (_.2 . _.3) (a.0 . 1) . _.4) : ((a.0 . _.2) (a.0 . _.0)))))

(test "lookupo-5"
  (run 3 (q)
    (exist (env val)
      (== (list env val) q)
      (fresh (z)
        (lookupo z env val))))
  '((((a.0 . _.0) . _.1) _.0)
    ((((_.0 . _.1) (a.0 . _.2) . _.3) _.2) : ((a.0 . _.0)))
    ((((_.0 . _.1) (_.2 . _.3) (a.0 . _.4) . _.5) _.4) : ((a.0 . _.2) (a.0 . _.0)))))

(test "lookupo-5b"
  (run 3 (q)
    (exist (env val)
      (fresh (z)
        (== (list env val) q)
        (lookupo z env val))))
  '((((a.0 . _.0) . _.1) _.0)
    ((((_.0 . _.1) (a.0 . _.2) . _.3) _.2) : ((a.0 . _.0)))
    ((((_.0 . _.1) (_.2 . _.3) (a.0 . _.4) . _.5) _.4) : ((a.0 . _.2) (a.0 . _.0)))))

(test "lookupo-6"
  (run 3 (q)
    (fresh (z)
      (exist (env val)
        (== (list env val) q)
        (lookupo z env val))))
  '((((a.0 . _.0) . _.1) _.0)
    ((((_.0 . _.1) (a.0 . _.2) . _.3) _.2) : ((a.0 . _.0)))
    ((((_.0 . _.1) (_.2 . _.3) (a.0 . _.4) . _.5) _.4) : ((a.0 . _.2) (a.0 . _.0)))))

(test "lookup-7"
  (run* (val)
    (fresh (a)
      (lookupo a `((,a . 1)) val)))
  '(1))

(test "eval-expro-0"
  (run* (q) (fresh (b) (eval-expro '() `(Lam ,(tie b `(Var ,b))) q)))
  '((Closure () (tie-tag a.0 (Var a.0)))))

(test "eval-expro-0b"
  (run* (q)
    (fresh (b)
      (eval-expro '() `(Lam ,(tie b `(Var ,b))) q))
    (fresh (c)
      (eval-expro '() `(Lam ,(tie c `(Var ,c))) q)))
  '((Closure () (tie-tag a.0 (Var a.0)))))

(test "eval-expro-0c"
  (run* (q)
    (fresh (b c d)
      (eval-expro '() `(App (Lam ,(tie b `(Lam ,(tie c `(Var ,c))))) (Lam ,(tie d `(Var ,d)))) q)))
  '((Closure ((a.0 Closure () (tie-tag a.1 (Var a.1)))) (tie-tag a.2 (Var a.2)))))

(test "eval-expro-0d"
  (run* (q)
    (fresh (b c d)
      (eval-expro '() `(App (Lam ,(tie b `(Lam ,(tie c `(Var ,b))))) (Lam ,(tie d `(Var ,d)))) q)))
  '((Closure ((a.0 Closure () (tie-tag a.1 (Var a.1)))) (tie-tag a.2 (Var a.0)))))

(test "eval-expro-0e"
  (run* (q)
    (fresh (b c)
      (eval-expro '() `(App (Lam ,(tie b `(Var ,b))) (Lam ,(tie c `(Var ,c)))) q)))
  '((Closure () (tie-tag a.0 (Var a.0)))))

(test "eval-expro-0f"
  (run 1 (expr)
    (fresh (a)
      (eval-expro '() expr `(Closure () ,(tie a `(Var ,a))))))
  '((Lam (tie-tag a.0 (Var a.0)))))

(test "eval-expro-0g"
  (run 2 (expr)
    (fresh (a)
      (eval-expro '() expr `(Closure () ,(tie a `(Var ,a))))))
  '((Lam (tie-tag a.0 (Var a.0)))
    (App (Lam (tie-tag a.0 (Var a.0)))
         (Lam (tie-tag a.1 (Var a.1))))))

#| ;; Ugh--this test fails if the (hash a y) goal comes before the recursive call in lookupo.
(test "eval-expro-0h"
  (run 3 (expr)
    (fresh (a)
      (eval-expro '() expr `(Closure () ,(tie a `(Var ,a))))))
  '???)
;; Exception in hash: first argument is not a nom with irritant (susp-tag () #<procedure at alphaKanren.scm:1915>)
;; Type (debug) to enter the debugger.
|#

(test "eval-expro-0i"
  (run 10 (expr)
    (fresh (a)
      (eval-expro '() expr `(Closure () ,(tie a `(Var ,a))))))
  '((Lam (tie-tag a.0 (Var a.0)))
    (App (Lam (tie-tag a.0 (Var a.0)))
         (Lam (tie-tag a.1 (Var a.1))))
    (App (Lam (tie-tag a.0 (App (Var a.0) (Var a.0))))
         (Lam (tie-tag a.1 (Var a.1))))
    (App (Lam (tie-tag a.0 (Var a.0)))
         (App (Lam (tie-tag a.1 (Var a.1)))
              (Lam (tie-tag a.2 (Var a.2)))))
    (App (App (Lam (tie-tag a.0 (Var a.0)))
              (Lam (tie-tag a.1 (Var a.1))))
         (Lam (tie-tag a.2 (Var a.2))))
    (App (Lam (tie-tag
               a.0
               (App (Lam (tie-tag a.1 (Var a.1))) (Var a.0))))
         (Lam (tie-tag a.2 (Var a.2))))
    (App (Lam (tie-tag a.0 (App (Var a.0) (Var a.0))))
         (App (Lam (tie-tag a.1 (Var a.1)))
              (Lam (tie-tag a.2 (Var a.2)))))
    (App (Lam (tie-tag
               a.0
               (App (Lam (tie-tag a.1 (Var a.0))) (Var a.0))))
         (Lam (tie-tag a.2 (Var a.2))))
    (App (App (Lam (tie-tag a.0 (Var a.0)))
              (Lam (tie-tag a.1 (App (Var a.1) (Var a.1)))))
         (Lam (tie-tag a.2 (Var a.2))))
    ((App (Lam (tie-tag
                a.0
                (App (Lam (tie-tag a.1 (Var a.0)))
                     (Lam (tie-tag a.2 (susp-tag ((a.3 a.0)) _.0))))))
          (Lam (tie-tag a.4 (Var a.4))))
     :
     ((a.0 . _.0)))))

(test "eval-expro-1"
  (run* (val)
    (fresh (a)
      (eval-expro '() `(Var ,a) val)))
  '())

(test "eval-expro-2"
  (run* (q)
    (fresh (a)
      (eval-expro `((,a . 1)) `(Var ,a) a)))
  '())

(test "eval-expro-3"
  (run* (val)
    (fresh (a)
      (eval-expro `((,a . 1)) `(Var ,a) val)))
  '(1))

(test "eval-expro-4a"
  (run* (val)
    (exist (x)
      (eval-expro `((,x . 1)) `(Var ,x) val)))
  '(1))

#|
(test "eval-expro-4b"
  (run* (val)
    (exist (x y)
      (eval-expro `((,x . 1) (,y . ,2)) `(Var ,x) val)))
  '(1))
;; Exception in hash: first argument is not a nom with irritant (susp-tag () #<procedure at alphaKanren.scm:1915>)
;; Type (debug) to enter the debugger.
|#

;; As long as the initial environment is either empty or has ground
;; noms for variables, `expr` and `val` can both be fresh)
(test "eval-expro-5"
  (length (run 500 (q)
            (exist (expr val)
              (== (list expr val) q)
              (eval-expro '() expr val))))
  500)

(test "eval-expro-6"
  (run* (q)
    (fresh (a a^)
      (exist (val)
        (== (list a a^ val) q)
        (eval-expro `((,a . (N (NVar ,a^)))) `(Var ,a) val))))
  '((a.0 a.1 (N (NVar a.1)))))

(test "eval-expro-7"
  (run* (q)
    (fresh (a b)
      (exist (val)
        (== (list a b val) q)
        (eval-expro
         '()
         `(App (Lam ,(tie a `(Lam ,(tie b `(Var ,a)))))
               (Lam ,(tie a `(Var ,a))))
         val))))
  '((a.0 a.1 (Closure ((a.2 Closure () (tie-tag a.3 (Var a.3)))) (tie-tag a.4 (Var a.2))))))


(test "main"
  (run* (result)
    (exist (id_ const_)
      (fresh (a)
        (eval-expro '() `(Lam ,(tie a `(Var ,a))) id_))
      (fresh (a b)
        (eval-expro '() `(Lam ,(tie a `(Lam ,(tie b `(Var ,a))))) const_))
      (fresh (a b)
        (eval-expro `((,a . ,id_) (,b . ,const_)) `(App (Var ,b) (Var ,a)) result))))
  '((Closure ((a.0 Closure () (tie-tag a.1 (Var a.1)))) (tie-tag a.2 (Var a.0)))))

(test "uneval-neutralo-1"
  (run 1 (q)
    (exist (n e)
      (== (list n e) q)
      (uneval-neutralo n e)))
  '(((NVar _.0) (Var _.0))))

(test "uneval-neutralo-2"
  (run 1 (q)
    (fresh (a)
      (uneval-neutralo `(NVar ,a) `(Var ,a))))
  '(_.0))

(test "uneval-neutralo-3"
  (run 1 (e)
    (fresh (a)
      (uneval-valueo `(N (NVar ,a)) e)))
  '((Var a.0)))

(test "uneval-valueo-1"
  (run 1 (q)
    (exist (v e)
      (== (list v e) q)
      (uneval-valueo v e)))
  '(((N (NVar _.0)) (Var _.0))))

(test "uneval-valueo-1b"
  (run 1 (e)
    (fresh (a)
      (uneval-valueo `(N (NVar ,a)) e)))
  '((Var a.0)))

(test "uneval-valueo-2"
  (run* (expr)
    (fresh (a)
      (uneval-valueo `(Closure () ,(tie a `(Var ,a))) expr)))
  '((Lam (tie-tag a.0 (Var a.0)))))

(test "uneval-valueo-3"
  (run* (expr)
    (fresh (a2 a3 a4)
      (uneval-valueo `(Closure ((,a2 . (Closure () ,(tie a3 `(Var ,a3))))) ,(tie a4 `(Var ,a2))) expr)))
  '((Lam (tie-tag a.0 (Lam (tie-tag a.1 (Var a.1)))))))

(test "eval-expro/uneval-valueo-1"
  (run* (result)
    (exist (id_)
      (fresh (a)
        (eval-expro '() `(Lam ,(tie a `(Var ,a))) id_))
      (uneval-valueo id_ result)))
  '((Lam (tie-tag a.0 (Var a.0)))))

;; WEB -- an important test!  Note the run*.
;;
;; If we just ran an `evalo`-like interpreter backwards to get the
;; original expression, given a closure, we would get infinitely many
;; values (including the application that we started from), rather
;; than a single normal form.
(test "eval-expro/uneval-valueo-2"
  (run* (result)
    (exist (val)
      (fresh (a b)
        (eval-expro
         '()
         `(App (Lam ,(tie a `(Lam ,(tie b `(Var ,a)))))
               (Lam ,(tie a `(Var ,a))))
         val))
      (uneval-valueo val result)))
  '((Lam (tie-tag a.0 (Lam (tie-tag a.1 (Var a.1)))))))

