(load "../scheme-helpers/pmatch.scm")
(load "../scheme-helpers/test-macro.scm")

;; just to be safe
(define eval 'eval-undefined)
(define apply 'apply-undefined)

(define lookup
  (lambda (x env)
    (pmatch env
      [() (error 'lookup (format "Unbound variable ~s" x))]
      [((,y . ,v) . ,env^)
       (cond
         ((equal? x y) v)
         (else (lookup x env^)))])))

(define eval-expr
  (lambda (env expr)
    (pmatch expr
      [(Var ,x) (lookup x env)]
      [(App ,f ,x)
       (let ((vf (eval-expr env f)))
         (let ((vx (eval-expr env x)))
           (apply-expr vf vx)))]
      [(Lam ,x ,b) `(Closure ,env ,x ,b)])))

(define apply-expr
  (lambda (f v)
    (pmatch f
      [(Closure ,env ,x ,b)
       (eval-expr `((,x . ,v) . ,env) b)]
      [(N ,n) `(N (NApp ,n ,v))])))

(define fresh
  (lambda (xs x)
    (cond
      ((member x xs)
       (fresh xs (string->symbol (string-append (symbol->string x) "^"))))
      (else x))))

(define uneval-value
  (lambda (xs v)
    (pmatch v
      [(Closure ,env ,x ,b)
       (let ((x^ (fresh xs x)))
         (let ((bv (eval-expr `((,x . (N (NVar ,x^))) . ,env) b)))
           (let ((b^ (uneval-value `(,x^ . ,xs) bv)))
             `(Lam ,x^ ,b^))))]
      [(N ,n) (uneval-neutral xs n)])))

(define uneval-neutral
  (lambda (xs n)
    (pmatch n
      [(NVar ,x^) `(Var ,x^)]
      [(NApp ,n ,v)
       (let ((ne (uneval-neutral xs n)))
         (let ((ve (uneval-value xs v)))
           `(App ,ne ,ve)))])))

(define nf
  (lambda (env t)
    (let ((v (eval-expr env t)))
      (uneval-value '() v))))

(define main
  (lambda ()
    (let ((id_ (eval-expr '() '(Lam x (Var x)))))
      (let ((const_ (eval-expr '() '(Lam x (Lam y (Var x))))))
        (let ((result (eval-expr `((id . ,id_) (const . ,const_)) '(App (Var const) (Var id)))))
          result)))))

(test "main"
  (main)
  '(Closure ((x Closure () x (Var x))) y (Var x)))

;; nf [] (Lam "x" (App (Lam "y" (App (Var "x") (Var "y"))) (Lam "x" (Var "x"))))
;; =>
;; Lam "x" (App (Var "x") (Lam "x'" (Var "x'")))
(test "nf-0"
  (nf '() '(Lam x (App (Lam y (App (Var x) (Var y))) (Lam x (Var x)))))
  '(Lam x (App (Var x) (Lam x^ (Var x^)))))

