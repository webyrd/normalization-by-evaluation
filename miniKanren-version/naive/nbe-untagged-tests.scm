(load "nbe-untagged.scm")
(load "../faster-miniKanren/test-check.scm")

(test "main"
  (run* (result)
    (fresh (id_ const_)
      (eval-expro '(lambda (x) x) '() id_)
      (eval-expro '(lambda (x) (lambda (y) x)) '() const_)
      (eval-expro '(App const id) `((id . ,id_) (const . ,const_)) result)))
  '((Closure y x ((x Closure x x ())))))

(test "eval-expro-1"
  (run* (val)
    (eval-expro `(lambda (z) z) '() val))
  '((Closure z z ())))

(test "eval-expro-2"
  (run* (val)
    (eval-expro
     `(App (lambda (x) (lambda (y) x))
           (lambda (z) z))
     '()
     val))
  '((Closure y x ((x Closure z z ())))))

(test "uneval-valueo-0"
  (run 6 (val expr)
    (uneval-valueo '() val expr))
  '((((N (NVar _.0))
      _.0)
     (sym _.0))
    (((N (NApp (NVar _.0) (N (NVar _.1))))
      (App _.0 _.1))
     (sym _.0 _.1))
    (((Closure _.0 _.0 _.1)
      (lambda (_.2) _.2))
     (sym _.0 _.2))
    (((N (NApp (NApp (NVar _.0) (N (NVar _.1))) (N (NVar _.2))))
      (App (App _.0 _.1) _.2))
     (sym _.0 _.1 _.2))
    (((Closure _.0 _.1 ((_.1 N (NVar _.2)) . _.3))
      (lambda (_.4) _.2))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1 _.2 _.4))
    (((Closure _.0 (lambda (_.1) _.1) _.2)
      (lambda (_.3) (lambda (_.4) _.4)))
     (=/= ((_.3 _.4)))
     (sym _.0 _.1 _.3 _.4))))

(test "uneval-valueo-1"
  (run* (expr)
    (uneval-valueo '() '(Closure y x ((x Closure z z ()))) expr))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-2"
  (run* (result)
    (fresh (val)
      (eval-expro
       `(App (lambda (x) (lambda (y) x))
             (lambda (z) z))
       '()
       val)
      (uneval-valueo '() val result)))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))


(test "eval-expro/uneval-valueo-3"
  (run* (expr)
    (nfo
     `(lambda (y)
           (App (lambda (x) (lambda (y) x))
                y))
     '()
     expr))
  '(((lambda (_.0) (lambda (_.1) _.0))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-4"
  (run* (q)
    (nfo
     `(lambda (y)
           (App (lambda (x) (lambda (y) x))
                y))
     '()
     `(lambda (y) (lambda (y) y))))
  '())

(test "eval-expro/uneval-valueo-4-expressed"
  (run* (q)
    (nfo
     `(lambda (y)
           (App (lambda (x) (lambda (y) x))
                y))
     '()
     `(lambda (y) (lambda (z) y))))
  '(_.0))
