(load "alphaKanren/alphaKanren.scm")

;; The first argument to lookupo *must* be a nom.
(define lookupo
  (lambda (a env val)
    (exist (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (conde
        ((== a y) (== v val))
        ((hash a y) ;; a =/= y
         (lookupo a env^ val))))))

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
           (== `(Closure ,env ,a ,body) val)))))))

#|
(run 1 (Y)
  (fresh (z f x)
    (exist (U)
      (== `(lam ,(tie f `(app ,U ,U))) Y)
      (hash z Y)
      (step-equalo `(app (var ,z) (app ,Y (var ,z))) `(app ,Y (var ,z))))))
|#

#|
(define substo
  (lambda (id/tm E out)
    (conde
      ((fresh (a)
         (== (tie a `(var ,a)) id/tm)
         (== E out)))
      ((fresh (a)
         (exist (B)
           (hash a B)
           (== (tie a `(var ,B)) id/tm)
           (== `(var ,B) out))))
      ((fresh (a b)
         (exist (E1 E1^)
           (hash b E)
           (== (tie a `(lam ,(tie b E1))) id/tm)
           (== `(lam ,(tie b E1^)) out)
           (substo (tie a E1) E E1^))))
      ((fresh (a)
         (exist (E1 E2 E1^ E2^)
           (== (tie a `(app ,E1 ,E2)) id/tm)
           (== `(app ,E1^ ,E2^) out)
           (substo (tie a E1) E E1^)
           (substo (tie a E2) E E2^)))))))
|#
