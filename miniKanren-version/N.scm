;; load mk

(define lookupo
  (lambda (x env val)
    (fresh (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (conde
        ((== x y) (== v val))
        ((=/= x y)
         (lookupo x env^ v))))))

(define eval-expro
  (lambda (env expr val)
    (conde
      ((fresh (x)
         (== `(Var ,x) expr)
         (lookupo x env val)))
      ((fresh (f x vf vx)
         (== `(App ,f ,x) expr)
         (eval-expro env f vf)
         (eval-expro env x vx)
         (apply-expro vf vx val)))
      ((fresh (x b)
         (== `(Lam ,x ,b) expr)
         (== `(Closure ,env ,x ,b) val))))))

(define apply-expro
  (lambda (f v val)
    (conde
      ((fresh (env x b)
         (== `(Closure ,env ,x ,b) f)
         (eval-expro `((,x . ,v) . ,env) b val)))
      ((fresh (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val))))))

#|
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
         (let ((bv (eval-expr `((,x . (N (Nvar ,x^))) . ,env) b)))
           (let ((b^ (uneval-value `((,x^ . ,xs)) bv)))
             `(Lam x^ b^))))]
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

(printf "~s\n" (main))
|#
