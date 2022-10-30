(load "nbe-extended.scm")
(load "../../scheme-helpers/test-macro.scm")


(test "parse-1"
  (parse '(lambda (x) (null? x)))
  '(Lam (null? (Var z))))

(test "parse-2"
  (parse '((lambda (f)
             ((lambda (x) (f (lambda (v) ((x x) v))))
              (lambda (x) (f (lambda (v) ((x x) v))))))
           (lambda (list?)
             (lambda (l)
               (if (null? l)
                   #t
                   (if (pair? l)
                       (list? (cdr l))
                       #f))))))
  '(App (Lam (App (Lam (App (Var (s z))
                            (Lam (App (App (Var (s z)) (Var (s z)))
                                      (Var z)))))
                  (Lam (App (Var (s z))
                            (Lam (App (App (Var (s z)) (Var (s z)))
                                      (Var z)))))))
        (Lam (Lam (if (null? (Var z))
                      #t
                      (if (pair? (Var z))
                          (App (Var (s z)) (cdr (Var z)))
                          #f))))))

(test "parse-3"
  (parse '(((lambda (f)
              ((lambda (x) (f (lambda (v) ((x x) v))))
               (lambda (x) (f (lambda (v) ((x x) v))))))
            (lambda (list?)
              (lambda (l)
                (if (null? l)
                    #t
                    (if (pair? l)
                        (list? (cdr l))
                        #f)))))
           (cons 1 (cons 2 (cons 3 '())))))
  '(App (App (Lam (App (Lam (App (Var (s z))
                                 (Lam (App (App (Var (s z)) (Var (s z)))
                                           (Var z)))))
                       (Lam (App (Var (s z))
                                 (Lam (App (App (Var (s z)) (Var (s z)))
                                           (Var z)))))))
             (Lam (Lam (if (null? (Var z))
                           #t
                           (if (pair? (Var z))
                               (App (Var (s z)) (cdr (Var z)))
                               #f)))))
        (cons 1 (cons 2 (cons 3 '())))))

(test "parse/evalo-list?-1"
  (let ((prog (parse '(((lambda (f)
                          ((lambda (x) (f (lambda (v) ((x x) v))))
                           (lambda (x) (f (lambda (v) ((x x) v))))))
                        (lambda (list?)
                          (lambda (l)
                            (if (null? l)
                                #t
                                (if (pair? l)
                                    (list? (cdr l))
                                    #f)))))
                       (cons 1 (cons 2 (cons 3 '())))))))
    (run* (q) (evalo '() prog q)))
  '(#t))

(test "parse/evalo-list?-2"
  (let ((prog (parse '(((lambda (f)
                          ((lambda (x) (f (lambda (v) ((x x) v))))
                           (lambda (x) (f (lambda (v) ((x x) v))))))
                        (lambda (list?)
                          (lambda (l)
                            (if (null? l)
                                #t
                                (if (pair? l)
                                    (list? (cdr l))
                                    #f)))))
                       (cons 1 (cons 2 (cons 3 4)))))))
    (run* (q) (evalo '() prog q)))
  '(#f))

(test "parse/evalo-list?-3"
  (let ((prog (parse '((lambda (f)
                         ((lambda (x) (f (lambda (v) ((x x) v))))
                          (lambda (x) (f (lambda (v) ((x x) v))))))
                       (lambda (list?)
                         (lambda (l)
                           (if (null? l)
                               #t
                               (if (pair? l)
                                   (list? (cdr l))
                                   #f))))))))
    (run* (q) (evalo '() prog q)))
  '((Clo ((Clo ((Clo ((Clo ()
                           (Lam (if (null? (Var z))
                                    #t
                                    (if (pair? (Var z))
                                        (App (Var (s z)) (cdr (Var z)))
                                        #f)))))
                     (App (Var (s z))
                          (Lam (App (App (Var (s z)) (Var (s z))) (Var z)))))
                (Clo ()
                     (Lam (if (null? (Var z))
                              #t
                              (if (pair? (Var z))
                                  (App (Var (s z)) (cdr (Var z)))
                                  #f)))))
               (App (App (Var (s z)) (Var (s z))) (Var z))))
         (if (null? (Var z))
             #t
             (if (pair? (Var z)) (App (Var (s z)) (cdr (Var z))) #f)))))


(test "nfo-0"
  (run* (q) (nfo '() '(quote ()) q))
  '((quote ())))

(test "nfo-1"
  (run* (q) (nfo '() '5 q))
  '(5))

(test "nfo-2"
  (run* (q) (nfo '() '#f q))
  '(#f))

(test "nfo-3"
  (run* (q) (nfo '() '#t q))
  '(#t))

(test "nfo-4"
  (run* (q) (nfo '() '(cons 3 4) q))
  '((cons 3 4)))

(test "nfo-5"
  (run* (q) (nfo '() '(null? 4) q))
  '(#f))

(test "nfo-6"
  (run* (q) (nfo '() '(null? (quote ())) q))
  '(#t))

(test "nfo-7"
  (run* (q) (nfo '() '(car (cons 3 4)) q))
  '(3))

(test "nfo-8"
  (run* (q) (nfo '() '(cdr (cons 3 4)) q))
  '(4))

(test "nfo-9"
  (run* (q) (nfo '() '(if (null? 3) 4 5) q))
  '(5))

(test "nfo-10"
  (run* (q) (nfo '() '(if (null? (quote ())) 4 5) q))
  '(4))


(test "parse/nfo-list?-3b1"
  (let ((prog (parse '(lambda (list?)
                        (lambda (l)
                          (if (null? l)
                              #t
                              #f))))))
    (run 1 (q) (nfo '() prog q)))
  '((Lam (Lam #f))))

;; why does this return ()?
(test "parse/nfo-list?-3b3"
  (let ((prog (parse '(lambda (l)
                        (cdr l)))))
    (run 1 (q) (nfo '() prog q)))
  '???)

(test "parse/evalo-list?-3b3"
  (let ((prog (parse '(lambda (l)
                        (cdr l)))))
    (run 1 (q) (evalo '() prog q)))
  '((Clo () (cdr (Var z)))))

;; why does this return ()?
(test "parse/nfo-list?-3b2"
  (let ((prog (parse '(lambda (list?)
                        (lambda (l)
                          (cdr l))))))
    (run 1 (q) (nfo '() prog q)))
  '???)

(test "parse/nfo-list?-3b3a"
  (let ((prog (parse '(lambda (l)
                        (cons l l)))))
    (run 1 (q) (nfo '() prog q)))
  '((Lam (cons (Var z) (Var z)))))

(test "parse/nfo-list?-3b4"
  (let ((prog (parse '(lambda (l)
                        l))))
    (run 1 (q) (nfo '() prog q)))
  '((Lam (Var z))))

;; why does this return ()?
(test "parse/nfo-list?-3b"
  (let ((prog (parse '(lambda (list?)
                        (lambda (l)
                          (if (null? l)
                              #t
                              (if (pair? l)
                                  (list? (cdr l))
                                  #f)))))))
    (run 1 (q) (nfo '() prog q)))
  '???)

;; why does this return ()?
(test "parse/nfo-list?-3"
  (let ((prog (parse '((lambda (f)
                         ((lambda (x) (f (lambda (v) ((x x) v))))
                          (lambda (x) (f (lambda (v) ((x x) v))))))
                       (lambda (list?)
                         (lambda (l)
                           (if (null? l)
                               #t
                               (if (pair? l)
                                   (list? (cdr l))
                                   #f))))))))
    (run* (q) (nfo '() prog q)))
  '???)

;; This seems to diverge!  Why?
(test "parse/nfo-list?-3a"
  (let ((prog (parse '(lambda (f)
                        ((lambda (x) (f (lambda (v) ((x x) v))))
                         (lambda (x) (f (lambda (v) ((x x) v)))))))))
    (run 1 (q) (nfo '() prog q)))
  '???)
