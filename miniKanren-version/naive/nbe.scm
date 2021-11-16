(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/test-check.scm")

(define lookupo
  (lambda (x env val)
    (fresh (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (symbolo x)
      (symbolo y)
      (conde
        ((== x y) (== v val))
        ((=/= x y)
         (lookupo x env^ val))))))

(define eval-expro
  (lambda (expr env val)
    (conde
      ((fresh (x body)
         (== `(Lam ,x ,body) expr)
         (== `(Closure ,x ,body ,env) val)
         (symbolo x)))
      ((fresh (x)
         (== `(Var ,x) expr)
         (symbolo x)
         (lookupo x env val)))
      ((fresh (e1 e2 f v)
         (== `(App ,e1 ,e2) expr)
         (eval-expro e1 env f)
         (eval-expro e2 env v)
         (apply-expro f v val))))))

(define apply-expro
  (lambda (f v val)
    (conde
      ((fresh (x body env)
         (== `(Closure ,x ,body ,env) f)
         (symbolo x)
         (eval-expro body `((,x . ,v) . ,env) val)))
      ((fresh (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val))))))

;; Fast and simple fresho definition (written with Michael Ballantyne)
;; Rather than compute a renamed variable, we just describe the constraints.
(define fresho
  (lambda (xs x^)
    (fresh ()
      (symbolo x^)
      (absento x^ xs))))

(define uneval-valueo
  (lambda (xs v expr)
    (conde
      ((fresh (x body env x^ body^ bv)
         (== `(Closure ,x ,body ,env) v)
         (== `(Lam ,x^ ,body^) expr)
         (symbolo x)
         (symbolo x^)
         (fresho xs x^)
         (eval-expro body `((,x . (N (NVar ,x^))) . ,env) bv)
         (uneval-valueo `(,x^ . ,xs) bv body^)))
      ((fresh (n)
         (== `(N ,n) v)
         (uneval-neutralo xs n expr))))))

(define uneval-neutralo
  (lambda (xs n expr)
    (conde
      ((fresh (x)
         (== `(NVar ,x) n)
         (== `(Var ,x) expr)))
      ((fresh (n^ v ne ve)
         (== `(NApp ,n^ ,v) n)         
         (== `(App ,ne ,ve) expr)
         (uneval-neutralo xs n^ ne)
         (uneval-valueo xs v ve))))))

(define nfo
  (lambda (t env expr)
    (fresh (v)
      (eval-expro t env v)
      (uneval-valueo '() v expr))))

(define main
  (lambda ()
    (run* (result)
      (fresh (id_ const_)
        (eval-expro '(Lam x (Var x)) '() id_)
        (eval-expro '(Lam x (Lam y (Var x))) '() const_)
        (eval-expro '(App (Var const) (Var id)) `((id . ,id_) (const . ,const_)) result)))))


(test "main"
  (main)
  '((Closure y (Var x) ((x Closure x (Var x) ())))))

;; nf [] (Lam "x" (App (Lam "y" (App (Var "x") (Var "y"))) (Lam "x" (Var "x"))))
;; =>
;; Lam "x" (App (Var "x") (Lam "x'" (Var "x'")))
(test "nf-0"
  (run* (expr)
    (nfo '(Lam x (App (Lam y (App (Var x) (Var y))) (Lam x (Var x)))) '() expr))
  '(((Lam _.0 (App (Var _.0) (Lam _.1 (Var _.1))))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))
