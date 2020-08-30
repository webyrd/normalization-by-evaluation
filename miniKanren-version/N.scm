;; load mk

(define lookupo
  (lambda (x env val)
    (fresh (y v env^)
      (symbolo x)
      (symbolo y)
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
         (symbolo x)
         (lookupo x env val)))
      ((fresh (f x vf vx)
         (== `(App ,f ,x) expr)
         (eval-expro env f vf)
         (eval-expro env x vx)
         (apply-expro vf vx val)))
      ((fresh (x b)
         (== `(Lam ,x ,b) expr)
         (symbolo x)
         (== `(Closure ,env ,x ,b) val))))))

(define apply-expro
  (lambda (f v val)
    (conde
      ((fresh (env x b)
         (== `(Closure ,env ,x ,b) f)
         (symbolo x)
         (eval-expro `((,x . ,v) . ,env) b val)))
      ((fresh (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val))))))

(define fresho
  (lambda (xs x x^)
    (fresh ()
      (symbolo x)
      (symbolo x^)
      (conde
        ((membero x xs)
         (fresh (x^^)
           (fresho xs x^^ x^)))
        ((== x x^)
         (not-membero x xs))))))

(define membero
  (lambda (x ls)
    (fresh (y ls^)
      (== `((,y . ,ls^)) ls)
      (conde
        ((== x y))
        ((=/= x y) (membero x ls^))))))

(define not-membero
  (lambda (x ls)
    (conde
      ((== '() ls))
      ((fresh (y ls^)
         (== `((,y . ,ls^)) ls)
         (=/= x y)
         (not-membero x ls^))))))

(define uneval-valueo
  (lambda (xs v expr)
    (conde
      ((fresh (env x b x^ bv b^)
         (== `(Closure ,env ,x ,b) v)
         (symbolo x)
         (symbolo x^)
         (fresho xs x x^)
         (eval-expro `((,x . (N (Nvar ,x^))) . ,env) b bv)
         (uneval-valueo `((,x^ . ,xs)) bv b^)
         (== `(Lam ,x^ ,b^) expr)))
      ((fresh (n)
         (== `(N ,n) v)
         (uneval-neutralo xs n expr))))))

#|
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
