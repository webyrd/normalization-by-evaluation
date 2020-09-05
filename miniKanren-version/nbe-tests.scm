(load "nbe.scm")
(load "../scheme-helpers/test-macro.scm")

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