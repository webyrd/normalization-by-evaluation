(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")

(define ntho
  (lambda (n xs val)
    (conde
      ((== 'z n)
       (fresh (rest)
         (== `(,val . ,rest) xs)))
      ((fresh (n-1 y rest)
         (== `(s ,n-1) n)
         (== `(,y . ,rest) xs)
         (ntho n-1 rest val))))))

(define evalo
  (lambda (env expr val)
    (conde
      ((fresh (body)
         (== `(Lam ,body) expr)
         (== `(Clo ,env ,body) val)))
      ((fresh (x)
         (== `(Var ,x) expr)
         (ntho x env val)))
      ((fresh (f x fv xv)
         (== `(App ,f ,x) expr)
         (evalo env f fv)
         (evalo env x xv)
         (appo fv xv val))))))

(define appo
  (lambda (f v val)
    (conde
      ((fresh (n)
         (== `(N ,n) f)
         (== `(N (NApp ,n ,v)) val)))
      ((fresh (env body)
         (== `(Clo ,env ,body) f)
         (evalo `(,v . ,env) body val))))))

(define unevalo
  (lambda (d val expr)
    (conde
      ((fresh (n)
         (== `(N ,n) val)
         (unevalNo d n expr)))
      ((fresh (env body v expr^)
         (== `(Clo ,env ,body) val)
         (== `(Lam ,expr^) expr)
         (evalo `((N (NVar ,d)) . ,env) body v)
         (unevalo `(s ,d) v expr^))))))

(define unevalNo
  (lambda (d n expr)
    (conde
      ((fresh (n^ d-1 d-n-1)
         (== `(NVar ,n^) n)
         (== `(Var ,d-n-1) expr)
         (== `(s ,d-1) d)
         (minuso d-1 n^ d-n-1)))
      ((fresh (f x fe xe)
         (== `(Napp ,f ,x) n)
         (unevalNo d f fe)
         (unevalNo d x xe)
         (== `(App ,fe ,xe) expr))))))

(define minuso
  (lambda (n m n-m)
    (conde
      ((== 'z m) (== n n-m))
      ((fresh (m-1 n-1)
         (== `(s ,m-1) m)
         (== `(s ,n-1) n)
         (minuso n-1 m-1 n-m))))))

(define nfo
  (lambda (env expr expr^)
    (fresh (v)
      (evalo env expr v)
      (unevalo 'z v expr^))))

(define main
  (lambda ()
    (run 1 (expr^)
      (fresh (id_ const_)
        (evalo '() `(Lam (Var z)) id_)
        (evalo '() `(Lam (Lam (Var (s z)))) const_)
        (nfo `(,id_ ,const_) `(App (Var (s z)) (Var z)) expr^)))))
