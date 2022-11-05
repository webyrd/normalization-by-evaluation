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
         (== `(lambda (,x) ,body) expr)
         (== `(Closure ,x ,body ,env) val)
         (symbolo x)))
      ((symbolo expr) (lookupo expr env val))
      ((fresh (e1 e2 f v)
         (== `(App ,e1 ,e2) expr)
         (eval-expro e1 env f)
         (eval-expro e2 env v)
         (apply-expro f v val))))))

(define apply-expro
  (lambda (f v val)
    (conde
      ((fresh (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val)))
      ((fresh (x body env)
         (== `(Closure ,x ,body ,env) f)
         (symbolo x)
         (eval-expro body `((,x . ,v) . ,env) val))))))

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
      ((fresh (n)
         (== `(N ,n) v)
         (uneval-neutralo xs n expr)))
      ((fresh (x body env x^ body^ bv)
         (== `(Closure ,x ,body ,env) v)
         (== `(lambda (,x^) ,body^) expr)
         (symbolo x)
         (symbolo x^)
         (fresho xs x^)
         (eval-expro body `((,x . (N (NVar ,x^))) . ,env) bv)
         (uneval-valueo `(,x^ . ,xs) bv body^))))))

(define uneval-neutralo
  (lambda (xs n expr)
    (conde
      ((== `(NVar ,expr) n)
       (symbolo expr))
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
        (eval-expro '(lambda (x) x) '() id_)
        (eval-expro '(lambda (x) (lambda (y) x)) '() const_)
        (eval-expro '(App const id) `((id . ,id_) (const . ,const_)) result)))))


(test "main"
  (main)
  '((Closure y x ((x Closure x x ())))))

;; nf [] (Lam "x" (App (Lam "y" (App (Var "x") (Var "y"))) (Lam "x" (Var "x"))))
;; =>
;; Lam "x" (App (Var "x") (Lam "x'" (Var "x'")))
(test "nf-0"
  (run* (expr)
    (nfo '(lambda (x) (App (lambda (y) (App x y)) (lambda (x) x))) '() expr))
  '(((lambda (_.0) (App _.0 (lambda (_.1) _.1)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))
