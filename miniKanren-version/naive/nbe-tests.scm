(load "nbe.scm")
(load "../../scheme-helpers/test-macro.scm")

(test "main"
  (run* (result)
    (fresh (id_ const_)
      (eval-expro '(Lam x (Var x)) '() id_)
      (eval-expro '(Lam x (Lam y (Var x))) '() const_)
      (eval-expro '(App (Var const) (Var id)) `((id . ,id_) (const . ,const_)) result)))
  '((Closure y (Var x) ((x Closure x (Var x) ())))))

(test "membero-1"
  (run* (x) (membero x `(,x)))
  '(_.0))

(test "membero-2"
  (run* (x) (membero x '()))
  '())

(test "membero-3"
  (run* (x y) (membero x `(,y)))
  '((_.0 _.0)))

(test "not-membero-1"
  (run* (x) (not-membero x `(,x)))
  '())

(test "non-membero-2"
  (run* (x y) (not-membero x `(,y)))
  '(((_.0 _.1) (=/= ((_.0 _.1))))))

(test "not-membero-3"
  (run* (x) (not-membero x '()))
  '(_.0))

(test "fresho-1"
  (run* (x x^) (fresho '() x x^))
  '(((_.0 _.0) (sym _.0))))

(test "fresho-2"
  (run* (x x^) (fresho `(,x) x x^))
  '(((_.0 _.1) (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "fresho-3"
  (run* (x) (fresho `(,x) x x))
  '())

(test "fresho-4"
  (run* (x x^ y) (fresho `(,y) x x^))
  '(((_.0 _.0 _.1) (=/= ((_.0 _.1))) (sym _.0))
    ((_.0 _.1 _.0) (=/= ((_.0 _.1))) (sym _.0 _.1))))



(test "eval-expro-1"
  (run* (val)
    (eval-expro `(Lam z (Var z)) '() val))
  '((Closure z (Var z) ())))

(test "eval-expro-2"
  (run* (val)
    (eval-expro
     `(App (Lam x (Lam y (Var x)))
           (Lam z (Var z)))
     '()
     val))
  '((Closure y (Var x) ((x Closure z (Var z) ())))))

(test "uneval-valueo-0"
  (run 6 (val expr)
    (uneval-valueo '() val expr))
  '(((N (NVar _.0))
     (Var _.0))
    ((N (NApp (NVar _.0) (N (NVar _.1))))
     (App (Var _.0) (Var _.1)))
    (((Closure _.0 (Var _.1) ((_.1 N (NVar _.2)) . _.3))
      (Lam _.0 (Var _.2)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))
    ((N (NApp (NApp (NVar _.0) (N (NVar _.1))) (N (NVar _.2))))
     (App (App (Var _.0) (Var _.1)) (Var _.2)))
    ((N (NApp (NVar _.0) (N (NApp (NVar _.1) (N (NVar _.2))))))
     (App (Var _.0) (App (Var _.1) (Var _.2))))
    (((Closure _.0 (Var _.1) ((_.2 . _.3) (_.1 N (NVar _.4)) . _.5))
      (Lam _.0 (Var _.4)))
     (=/= ((_.0 _.1)) ((_.1 _.2)))
     (sym _.0 _.1 _.2))))

;; WEB TODO why doesn't this work?
(test "uneval-valueo-1"
  (run* (expr)
    (uneval-valueo '() expr '(Closure y (Var x) ((x Closure z (Var z) ())))))
  '???)


;; WEB TODO why doesn't this work?
;;
;; WEB -- does the nbe handle lexical scope and alpha-equivalence
;; properly?
;;
;; Does the code properly handle things like:
;;
;; ((lambda (x) (lambda (x) x)) (lambda (x) x))
(test "eval-expro/uneval-valueo-2"
  (run* (result)
    (fresh (val)
      (eval-expro
       `(App (Lam x (Lam y (Var x)))
             (Lam z (Var z)))
       '()
       val)
      (uneval-valueo '() val result)))
  '???)

#|
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
|#
