(load "nbe-untagged-extended-infer.scm")
(load "../faster-miniKanren/test-check.scm")

(test "infer-if-1"
  (run* (q) (!-o '() '(if (null? (quote (5 6))) #f #t) q))
  '(Bool))

(test "infer-if-2"
  (run* (q) (!-o '() '(if (null? (quote (5 6))) #f 5) q))
  '())

(test "infer-if-3"
  (run* (q)
    (!-o '()
         '(if (null? (quote (5 6)))
              (cons 3 (quote ()))
              (cons 4 (quote ())))
         q))
  '((List Nat)))

(test "infer-if-4"
  (run* (q) (!-o '() '(if 5 #f #t) q))
  '())


(test "infer-Bool-1"
  (run* (q) (!-o '() '#f q))
  '(Bool))

(test "infer-Bool-2"
  (run* (q) (!-o '() '#t q))
  '(Bool))

(test "infer-Nat-2"
  (run* (q) (!-o '() '5 q))
  '(Nat))

(test "infer-var-1"
  (run* (q) (!-o '() 'x q))
  '())

(test "infer-Ann-1"
  (run* (q) (!-o '() '(Ann (lambda (x) x) (-> Nat Nat)) q))
  '((-> Nat Nat)))

(test "infer-Ann-2"
  (run* (q) (!-o '() '(Ann (lambda (x) 5) (-> Bool Nat)) q))
  '((-> Bool Nat)))

(test "infer-Ann-3"
  (run* (q) (!-o '() '(Ann (lambda (x) 5) (-> Nat Nat)) q))
  '((-> Nat Nat)))

(test "infer-Ann-4"
  (run* (q) (!-o '() '(Ann (lambda (x) (car x)) (-> Nat Nat)) q))
  '())

(test "infer-Ann-5"
  (run* (q) (!-o '() '(Ann (lambda (x) (car x)) (-> (List Nat) Nat)) q))
  '((-> (List Nat) Nat)))

(test "infer-Ann-6"
  (run* (q) (!-o '() '(Ann (lambda (x) (cons 3 x)) (-> (List Nat) (List Nat))) q))
  '((-> (List Nat) (List Nat))))

(test "infer-Ann-7"
  (run* (q)
    (!-o '()
         '(Ann (lambda (x) (lambda (y) (cons x y)))
               (-> Nat (-> (List Nat) (List Nat))))
         q))
  '((-> Nat (-> (List Nat) (List Nat)))))

(test "infer-Ann-8"
  (run* (q)
    (!-o '()
         '(Ann (lambda (f) (f 5))
               (-> (-> Nat Bool) Bool))
         q))
  '((-> (-> Nat Bool) Bool)))

(test "infer-Ann-9"
  (run* (q)
    (!-o '()
         '(Ann (lambda (n) (lambda (f) (f n)))
               (-> Nat (-> (-> Nat Bool) Bool)))
         q))
  '((-> Nat (-> (-> Nat Bool) Bool))))

(test "infer-Ann-10"
  (run* (q)
    (!-o '()
         '((Ann (lambda (n) (lambda (f) (f n)))
                (-> Nat (-> (-> Nat Bool) Bool)))
           
           5)
         q))
  '((-> (-> Nat Bool) Bool)))

(test "infer-Ann-11"
  (run* (q)
    (!-o '()
         '((Ann (lambda (f) (lambda (n) (f n)))
                (-> (-> Nat Bool) (-> Nat Bool)))
           (Ann (lambda (y) #f)
                (-> Nat Bool)))
         q))
  '((-> Nat Bool)))

(test "infer-Ann-12"
  (run* (q)
    (!-o '()
         '(((Ann (lambda (f) (lambda (n) (f n)))
                 (-> (-> Nat Bool) (-> Nat Bool)))
            (Ann (lambda (y) #f)
                 (-> Nat Bool)))
           5)
         q))
  '(Bool))


(test "infer-quote-1"
  (run* (q) (!-o '() '(quote ()) q))
  '((List _.0)))

(test "infer-quote-2"
  (run* (q) (!-o '() '(quote 5) q))
  '(Nat))

(test "infer-quote-3"
  (run* (q) (!-o '() '(quote (5 6)) q))
  '((List Nat)))

(test "infer-quote-4"
  (run* (q) (!-o '() '(quote (#t #f)) q))
  '((List Bool)))

(test "infer-quote-5"
  (run* (q) (!-o '() '(quote (5 #f)) q))
  '())

(test "infer-quote-6"
  (run* (q) (!-o '() '(quote ((#t) (#f))) q))
  '((List (List Bool))))

(test "infer-quote-7"
  (run* (q) (!-o '() '(quote ((cat) (dog))) q))
  '((List (List Sym))))


(test "infer-null?-1"
  (run* (q) (!-o '() '(null? (quote ())) q))
  '(Bool))

(test "infer-pair?-1"
  (run* (q) (!-o '() '(pair? (quote ())) q))
  '(Bool))

(test "infer-number?-1"
  (run* (q) (!-o '() '(number? (quote ())) q))
  '(Bool))

(test "infer-symbol?-1"
  (run* (q) (!-o '() '(symbol? (quote ())) q))
  '(Bool))

(test "infer-list-1"
  (run* (q) (!-o '() '(cons 5 (quote ())) q))
  '((List Nat)))

(test "infer-list-2"
  (run* (q) (!-o '() '(cons 5 (cons 6 (quote ()))) q))
  '((List Nat)))

(test "infer-list-3"
  (run* (q) (!-o '() '(cons #f (cons #t (quote ()))) q))
  '((List Bool)))

(test "infer-list-4"
  (run* (q) (!-o '() '(cons 5 (cons #t (quote ()))) q))
  '())

(test "infer-list-5"
  (run* (q) (!-o '() '(cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ()))) q))
  '((List (List Nat))))

(test "infer-car-1"
  (run* (q) (!-o '() '(car (cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ())))) q))
  '((List Nat)))

(test "infer-car-2"
  (run* (q) (!-o '() '(car (quote ())) q))
  '(_.0))

(test "infer-car-3"
  (run* (q) (!-o '() '(car (quote 5)) q))
  '())

(test "infer-car-4"
  (run* (q) (!-o '() '(car (quote (5 6))) q))
  '(Nat))

(test "infer-cdr-1"
  (run* (q) (!-o '() '(cdr (cons (cons 5 (quote ())) (cons (cons 6 (quote ())) (quote ())))) q))
  '((List (List Nat))))

(test "infer-cdr-2"
  (run* (q) (!-o '() '(cdr (quote ())) q))
  '((List _.0)))

(test "infer-cdr-3"
  (run* (q) (!-o '() '(cdr (quote 5)) q))
  '())

(test "infer-cdr-4"
  (run* (q) (!-o '() '(cdr (quote (5 6))) q))
  '((List Nat)))
