(load "N.scm")
(load "../scheme-helpers/test-macro.scm")

;; WEB (fresh (a) (exist (x) (== a x) (hash x a))) signals an error,
;; since the first arg to `hash` is a nom, even though (fresh (a)
;; (exist (x) (== a x) (hash a a))) is legal.
;;
;; This feels overly restrictive.  Once we unify `x` and `a`, shouldn't
;; uses of `x` behave like uses of `a`?  Of course,
;; (fresh (a) (exist (x) (hash x a) (== a x))) should work the same as
;; (fresh (a) (exist (x) (== a x) (hash x a))).  How does alphaProlog
;; behave in this case?

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
