(load "alphaKanren/alphaKanren.scm")

(define lookupo
  (lambda (a env val)
    (exist (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (conde
        ((== a y) (== v val))
        ((lookupo a env^ val)
         ;; Non-relational warning!  This call to `hash` should come
         ;; after the call to lookupo, since the first argument to
         ;; hash *must* be a ground (a nom, or a unification variable
         ;; bound to nom)
         (hash a y) ;; a =/= y
         )))))

(define eval-expro
  (lambda (env expr val)
    (conde
      ((exist (x)
         (== `(Var ,x) expr)
         (lookupo x env val)))
      ((exist (e1 e2 f v)
         (== `(App ,e1 ,e2) expr)
         (eval-expro env e1 f)
         (eval-expro env e2 v)
         (apply-expro f v val)))
      ((fresh (a)
         (exist (body)
           (hash a env)
           (== `(Lam ,(tie a body)) expr)
           (== `(Closure ,env ,(tie a body)) val)))))))

(define apply-expro
  (lambda (f v val)
    (conde
      ((fresh (a)
         (exist (env body)
           (== `(Closure ,env ,(tie a body)) f)
           (eval-expro `((,a . ,v) . ,env) body val))))
      ((exist (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val))))))
