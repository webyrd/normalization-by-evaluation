(load "nbe-extended.scm")
(load "../../scheme-helpers/test-macro.scm")

(test "nfo-#f"
  (run* (q)
    (nfo '() (parse '#f) q))
  '(#f))

(test "nfo-#t"
  (run* (q)
    (nfo '() (parse '#t) q))
  '(#t))

(test "nfo-()-0"
  (run* (q)
    (nfo '() (parse '(quote ())) q))
  '((quote ())))

(test "nfo-cons-0"
  (run* (q)
    (nfo '() (parse '(cons 3 4)) q))
  '((cons 3 4)))

(test "nfo-cons-1"
  (run* (q)
    (nfo '() (parse '(cons 3 (quote ()))) q))
  '((cons 3 (quote ()))))

(test "nfo-cons-2"
  (run* (q)
    (nfo '() (parse '(cons (cons 4 5) (quote ()))) q))
  '((cons (cons 4 5) (quote ()))))

(test "nfo-cons-3"
  (run* (q)
    (nfo '() (parse '(lambda (x) (cons (cons x 5) x))) q))
  '((Lam (cons (cons (Var z) 5) (Var z)))))

(test "nfo-car-0"
  (run* (q)
    (nfo '() (parse '(car (cons 3 4))) q))
  '(3))

(test "nfo-car-1"
  (run* (q)
    (nfo '() (parse '(car (cons 3 (quote ())))) q))
  '(3))

(test "nfo-car-2"
  (run* (q)
    (nfo '() (parse '(car (cons (cons 4 5) (quote ())))) q))
  '((cons 4 5)))

(test "nfo-car-3"
  (run* (q)
    (nfo '() (parse '(lambda (x) (car (cons (cons x 5) x)))) q))
  '((Lam (cons (Var z) 5))))

(test "nfo-car-4"
  (run* (q)
    (nfo '() (parse '(lambda (x) (car x))) q))
  '((Lam (car (Var z)))))

(test "nfo-cdr-0"
  (run* (q)
    (nfo '() (parse '(cdr (cons 3 4))) q))
  '(4))

(test "nfo-cdr-1"
  (run* (q)
    (nfo '() (parse '(cdr (cons 3 (quote ())))) q))
  '((quote ())))

(test "nfo-cdr-2"
  (run* (q)
    (nfo '() (parse '(cdr (cons (cons 4 5) (quote ())))) q))
  '((quote ())))

(test "nfo-cdr-3"
  (run* (q)
    (nfo '() (parse '(lambda (x) (cdr (cons (cons x 5) x)))) q))
  '((Lam (Var z))))

(test "nfo-cdr-4"
  (run* (q)
    (nfo '() (parse '(lambda (x) (cdr x))) q))
  '((Lam (cdr (Var z)))))

(test "nfo-null?-0"
  (run* (q)
    (nfo '() (parse '(null? (quote ()))) q))
  '(#t))

(test "nfo-null?-1"
  (run* (q)
    (nfo '() (parse '(null? 5)) q))
  '(#f))

(test "nfo-null?-2"
  (run* (q)
    (nfo '() (parse '(null? #t)) q))
  '(#f))

(test "nfo-null?-3"
  (run* (q)
    (nfo '() (parse '(null? (lambda (x) x))) q))
  '(#f))

(test "nfo-null?-4"
  (run* (q)
    (nfo '() (parse '(null? (cons 3 4))) q))
  '(#f))

(test "nfo-pair?-0"
  (run* (q)
    (nfo '() (parse '(pair? (quote ()))) q))
  '(#f))

(test "nfo-pair?-1"
  (run* (q)
    (nfo '() (parse '(pair? 5)) q))
  '(#f))

(test "nfo-pair?-2"
  (run* (q)
    (nfo '() (parse '(pair? #t)) q))
  '(#f))

(test "nfo-pair?-3"
  (run* (q)
    (nfo '() (parse '(pair? (lambda (x) x))) q))
  '(#f))

(test "nfo-pair?-4"
  (run* (q)
    (nfo '() (parse '(pair? (cons 3 4))) q))
  '(#t))

(test "nfo-if-0"
  (run* (q)
    (nfo '() (parse '(if #f 5 6)) q))
  '(6))

(test "nfo-if-1"
  (run* (q)
    (nfo '() (parse '(if #t 5 6)) q))
  '(5))

(test "nfo-if-2"
  (run* (q)
    (nfo '() (parse '(if (if #t #f #t) 5 6)) q))
  '(6))

(test "nfo-if-3"
  (run* (q)
    (nfo '() (parse '(if (if #f #f #t) 5 6)) q))
  '(5))

(test "nfo-if-4"
  (run* (q)
    (nfo '() (parse '(if (if #f #f #t) (if #f 5 6) (if #t 7 8))) q))
  '(6))

(test "nfo-if-5"
  (run* (q)
    (nfo '() (parse '(if (if #t #f #t) (if #f 5 6) (if #t 7 8))) q))
  '(7))

(test "nfo-if-6"
  (run* (q)
    (nfo '() (parse '(lambda (x) (if x (if #f 5 6) (if #t 7 8)))) q))
  '((Lam (if (Var z) 6 7))))

(test "nfo-if/null?-0"
  (run* (q)
    (nfo '()
         (parse '(lambda (x) (if (null? x) (if #f 5 6) (if #t 7 8))))
         q))
  '((Lam (if (null? (Var z)) 6 7))))

(test "nfo-if/pair?-0"
  (run* (q)
    (nfo '()
         (parse '(lambda (x) (if (pair? x) (if #f 5 6) (if #t 7 8))))
         q))
  '((Lam (if (pair? (Var z)) 6 7))))
