(load "nbe.scm")
(load "../faster-miniKanren/test-check.scm")

(test "main"
  (run* (result)
    (fresh (id_ const_)
      (eval-expro '(Lam x (Var x)) '() id_)
      (eval-expro '(Lam x (Lam y (Var x))) '() const_)
      (eval-expro '(App (Var const) (Var id)) `((id . ,id_) (const . ,const_)) result)))
  '((Closure y (Var x) ((x Closure x (Var x) ())))))

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
    (((Closure _.0 (Var _.0) _.1)
      (Lam _.2 (Var _.2)))
     (sym _.0 _.2))
    (((Closure _.0 (Var _.1) ((_.1 N (NVar _.2)) . _.3))
      (Lam _.4 (Var _.2)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1 _.4))
    ((N (NApp (NApp (NVar _.0) (N (NVar _.1))) (N (NVar _.2))))
     (App (App (Var _.0) (Var _.1)) (Var _.2)))
    (((Closure _.0 (Lam _.1 (Var _.1)) _.2)
      (Lam _.3 (Lam _.4 (Var _.4))))
     (=/= ((_.3 _.4)))
     (sym _.0 _.1 _.3 _.4))))

(test "uneval-valueo-1"
  (run* (expr)
    (uneval-valueo '() '(Closure y (Var x) ((x Closure z (Var z) ()))) expr))
  '(((Lam _.0 (Lam _.1 (Var _.1)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-2"
  (run* (result)
    (fresh (val)
      (eval-expro
       `(App (Lam x (Lam y (Var x)))
             (Lam z (Var z)))
       '()
       val)
      (uneval-valueo '() val result)))
  '(((Lam _.0 (Lam _.1 (Var _.1)))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))

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

(test "eval-expro/uneval-valueo-3"
  (run* (expr)
    (nfo
     `(Lam y
           (App (Lam x (Lam y (Var x)))
                (Var y)))
     '()
     expr))
  '(((Lam _.0 (Lam _.1 (Var _.0)))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-4"
  (run* (q)
    (nfo
     `(Lam y
           (App (Lam x (Lam y (Var x)))
                (Var y)))
     '()
     `(Lam y (Lam y (Var y)))))
  '())

(test "eval-expro/uneval-valueo-4-expressed"
  (run* (q)
    (nfo
     `(Lam y
           (App (Lam x (Lam y (Var x)))
                (Var y)))
     '()
     `(Lam y (Lam z (Var y)))))
  '(_.0))
