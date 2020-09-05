(load "faster-miniKanren/mk-vicare.scm")
(load "faster-miniKanren/mk.scm")

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
         (== `(Lam ,x^ ,b^) expr)
         (symbolo x)
         (symbolo x^)
         (fresho xs x x^)
         (eval-expro `((,x . (N (Nvar ,x^))) . ,env) b bv)
         (uneval-valueo `((,x^ . ,xs)) bv b^)))
      ((fresh (n)
         (== `(N ,n) v)
         (uneval-neutralo xs n expr))))))

(define uneval-neutralo
  (lambda (xs n expr)
    (conde
      ((fresh (x^)
         (== `(NVar ,x^) n)
         (== `(Var ,x^) expr)))
      ((fresh (n^ v ne ve)
         (== `(NApp ,n^ ,v) n)         
         (== `(App ,ne ,ve) expr)
         (uneval-neutralo xs n^ ne)
         (uneval-valueo xs v ve))))))

(define nfo
  (lambda (env t expr)
    (fresh (v)
      (eval-expro env t v)
      (uneval-valueo '() v expr))))

(define main
  (lambda ()
    (run* (result)
      (fresh (id_ const_)
        (eval-expro '() '(Lam x (Var x)) id_)
        (eval-expro '() '(Lam x (Lam y (Var x))) const_)
        (eval-expro `((id . ,id_) (const . ,const_)) '(App (Var const) (Var id)) result)))))

(printf "~s\n" (main))
;; ((Closure ((x Closure () x (Var x))) y (Var x)))
