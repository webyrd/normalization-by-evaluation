(load "nbe-untagged-extended.scm")
(load "../faster-miniKanren/test-check.scm")

;; TODO
;;
;; * figure out if `nfo` Z combinator should diverge...
;; * support multi-argument `lambda`/closures and procedure application
;; * add `not-in-envo` to `evalo` to handle shadowing properly

;; TODO
;;
;; Nada Amin wonders (5 November 02022) if (lambda (x) x) and
;; (lambda (y) y) will normalize to the same expression.
;;
;; Taken more generally, this brings up many interesting questions.
;;
;; Try reproducing nominal-logic style tasks using nbe and our fresh approach
;;
;; Can we do capture-avoiding substitution?
;;
;; Can we do the things in the alphaKanren paper?
;;
;; Can we do the alphaLeanTAP things?
;;
;; Can we avoid the classic problems in miniKanren of conflating
;; (lambda (x) (lambda (y) x)) with (lambda (x) (lambda (y) y))?
;;
;; Can we ensure that (lambda (x) x) and (lambda (y) y) have the same normal form?
;;
;; Can we ensure that (lambda (x) (lambda (y) x)) and (lambda (y) (lambda (x) y))
;; have the same normal form, and that it differs from (lambda (x) (lambda (y) y))?
;;
;; Need to be careful, since 'nfo' will normalize the bodies of
;; lambdas, etc., which goes beyond alpha-equivalence.  Expressions
;; that are not alpha-equivalent may have the same normal form.
;; Can we still do useful and interesting things, given this behavior?
;; Or, can we use the freshness trick or whatever to implement just the
;; alpha-equivalence behavior, rather than normalization?


(test "nfo-alpha-1"
  (run* (e)
    (nfo '(lambda (x) x) '() e)
    (nfo '(lambda (x) x) '() e))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-alpha-2"
  (run* (e)
    (nfo '(lambda (x) x) '() e)
    (nfo '(lambda (y) y) '() e))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-alpha-3"
  (run* (e)
    (nfo '(lambda (x) (lambda (y) x)) '() e)
    (nfo '(lambda (x) (lambda (y) x)) '() e))
  '(((lambda (_.0) (lambda (_.1) _.0))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-alpha-4"
  (run* (e)
    (nfo '(lambda (x) (lambda (y) x)) '() e)
    (nfo '(lambda (x) (lambda (y) y)) '() e))
  '())


(test "main"
  (run* (result)
    (fresh (id_ const_)
      (eval-expro '(lambda (x) x) '() id_)
      (eval-expro '(lambda (x) (lambda (y) x)) '() const_)
      (eval-expro '(const id) `((id . ,id_) (const . ,const_)) result)))
  '((closure (y) x ((x . (closure (x) x ()))))))


(test "eval-expro-quine-1"
  (run 2 (q)
    (eval-expro q '() q))
  '(#f #t))

#|
;; sloooow with the extended language
(test "eval-expro-quine-2"
  (run 3 (q)
    (eval-expro q '() q))
  '(#f
    #t
    (((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))
|#

(test "nfo-quine-1"
  (run 14 (q)
    (nfo q '() q))
  '(#f
    #t
    (_.0
     (num _.0))
    '()
    ('_.0
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))
    '(#f . #f)
    ((lambda (_.0) #f)
     (sym _.0))
    '(#f . #t)
    ((lambda (_.0) #t)
     (sym _.0))
    '(#t . #f)
    ((lambda (_.0) _.1)
     (num _.1)
     (sym _.0))
    ('(#f . _.0)
     (num _.0))
    ((lambda (_.0) '())
     (sym _.0))
    '(#f)))

(test "nfo-U-1"
  (let ((U '(lambda (x) (x x))))
    (run 1 (e)
      (nfo U '() e)))
  '(((lambda (_.0) (_.0 _.0)) (sym _.0))))

#|
;; `nfo` of Omega seems to diverge.
;; This makes sense, since `nfo` first evaluates
;; Omega, which diverges.
(test "nfo-Omega-1"
  (let ((Omega '((lambda (x) (x x)) (lambda (x) (x x)))))
    (run 1 (e)
      (nfo Omega '() e)))
  '???)
|#

#|
;; `nfo` of Z combinator seems to diverge.
;; This might make sense, given that `nfo`
;; evaluates under `lambda`s, which may
;; result in self-application
(test "nfo-Z-combinator-1"
  (let ((Z '(lambda (f)
              ((lambda (x) (f (lambda (v) ((x x) v))))
               (lambda (x) (f (lambda (v) ((x x) v))))))))
    (run 1 (e)
      (nfo Z '() e)))
  '???)
|#


(test "nfo-#f-1"
  (run* (q)
    (nfo '#f '() q))
  '(#f))

(test "nfo-#f-2"
  (run* (q)
    (nfo '(quote #f) '() q))
  '(#f))

(test "nfo-#f-3"
  (run* (q)
    (nfo '((lambda (x) x) (quote #f)) '() q))
  '(#f))


(test "nfo-#t-1"
  (run* (q)
    (nfo '#t '() q))
  '(#t))

(test "nfo-#t-2"
  (run* (q)
    (nfo '(quote #t) '() q))
  '(#t))

(test "nfo-#t-3"
  (run* (q)
    (nfo '((lambda (x) x) (quote #t)) '() q))
  '(#t))


(test "nfo-number-1"
  (run* (q)
    (nfo '5 '() q))
  '(5))

(test "nfo-number-2"
  (run* (q)
    (nfo '(quote 5) '() q))
  '(5))

(test "nfo-number-3"
  (run* (q)
    (nfo '(lambda (x) (quote 5)) '() q))
  '(((lambda (_.0) 5)
     (sym _.0))))

(test "nfo-number-4"
  (run* (q)
    (nfo '(cons 5 6) '() q))
  '((quote (5 . 6))))

(test "nfo-number-5"
  (run* (q)
    (nfo '(cons 5 #f) '() q))
  '((quote (5 . #f))))

(test "nfo-number-6"
  (run* (q)
    (nfo '(cons #f 5) '() q))
  '((quote (#f . 5))))

(test "nfo-number-7"
  (run* (q)
    (nfo '(cons (quote #f) 5) '() q))
  '((quote (#f . 5))))

(test "nfo-number-8"
  (run* (q)
    (nfo '(cons (quote #f) (quote 5)) '() q))
  '((quote (#f . 5))))

(test "nfo-number-9"
  (run* (q)
    (nfo '(cons #f (quote 5)) '() q))
  '((quote (#f . 5))))

(test "nfo-number-10"
  (run* (q)
    (nfo '(lambda (x) (cons 5 (quote 5))) '() q))
  '(((lambda (_.0) '(5 . 5))
     (sym _.0))))

(test "nfo-number-11"
  (run* (q)
    (nfo '(lambda (x) (cons 5 #f)) '() q))
  '(((lambda (_.0) '(5 . #f))
     (sym _.0))))

(test "nfo-number-12"
  (run* (q)
    (nfo '(lambda (x) (cons 5 (quote ()))) '() q))
  '(((lambda (_.0) '(5 . ()))
     (sym _.0))))


(test "nfo-null?-0"
  (run* (q)
    (nfo '(null? (quote ())) '() q))
  '(#t))

(test "nfo-null?-1"
  (run* (q)
    (nfo '(null? 5) '() q))
  '(#f))

(test "nfo-null?-2"
  (run* (q)
    (nfo '(null? #t) '() q))
  '(#f))

(test "nfo-null?-3"
  (run* (q)
    (nfo '(null? (lambda (x) x)) '() q))
  '(#f))

(test "nfo-null?-4"
  (run* (q)
    (nfo '(null? (cons 3 4)) '() q))
  '(#f))

(test "nfo-null?-5"
  (run* (q)
    (nfo '(null? 'cat) '() q))
  '(#f))


(test "nfo-pair?-0"
  (run* (q)
    (nfo '(pair? (quote ())) '() q))
  '(#f))

(test "nfo-pair?-1"
  (run* (q)
    (nfo '(pair? 5) '() q))
  '(#f))

(test "nfo-pair?-2"
  (run* (q)
    (nfo '(pair? #t) '() q))
  '(#f))

(test "nfo-pair?-3"
  (run* (q)
    (nfo '(pair? (lambda (x) x)) '() q))
  '(#f))

(test "nfo-pair?-4"
  (run* (q)
    (nfo '(pair? (cons 3 4)) '() q))
  '(#t))

(test "nfo-pair?-5"
  (run* (q)
    (nfo '(pair? 'cat) '() q))
  '(#f))


(test "nfo-number?-0"
  (run* (q)
    (nfo '(number? (quote ())) '() q))
  '(#f))

(test "nfo-number?-1"
  (run* (q)
    (nfo '(number? 5) '() q))
  '(#t))

(test "nfo-number?-2"
  (run* (q)
    (nfo '(number? #t) '() q))
  '(#f))

(test "nfo-number?-3"
  (run* (q)
    (nfo '(number? (lambda (x) x)) '() q))
  '(#f))

(test "nfo-number?-4"
  (run* (q)
    (nfo '(number? (cons 3 4)) '() q))
  '(#f))

(test "nfo-number?-5"
  (run* (q)
    (nfo '(number? 'cat) '() q))
  '(#f))


(test "nfo-symbol?-0"
  (run* (q)
    (nfo '(symbol? (quote ())) '() q))
  '(#f))

(test "nfo-symbol?-1"
  (run* (q)
    (nfo '(symbol? 5) '() q))
  '(#f))

(test "nfo-symbol?-2"
  (run* (q)
    (nfo '(symbol? #t) '() q))
  '(#f))

(test "nfo-symbol?-3"
  (run* (q)
    (nfo '(symbol? (lambda (x) x)) '() q))
  '(#f))

(test "nfo-symbol?-4"
  (run* (q)
    (nfo '(symbol? (cons 3 4)) '() q))
  '(#f))

(test "nfo-symbol?-5"
  (run* (q)
    (nfo '(symbol? 'cat) '() q))
  '(#t))


(test "nfo-if-0"
  (run* (q)
    (nfo '(if #f 5 6) '() q))
  '(6))

(test "nfo-if-1"
  (run* (q)
    (nfo '(if #t 5 6) '() q))
  '(5))

(test "nfo-if-2"
  (run* (q)
    (nfo '(if (if #t #f #t) 5 6) '() q))
  '(6))

(test "nfo-if-3"
  (run* (q)
    (nfo '(if (if #f #f #t) 5 6) '() q))
  '(5))

(test "nfo-if-4"
  (run* (q)
    (nfo '(if (if #f #f #t) (if #f 5 6) (if #t 7 8)) '() q))
  '(6))

(test "nfo-if-5"
  (run* (q)
    (nfo '(if (if #t #f #t) (if #f 5 6) (if #t 7 8)) '() q))
  '(7))

(test "nfo-if-6"
  (run* (q)
    (nfo '(lambda (x) (if x (if #f 5 6) (if #t 7 8))) '() q))
  '(((lambda (_.0) (if _.0 6 7))
     (sym _.0))))

(test "nfo-if/null?-0"
  (run* (q)
    (nfo '(lambda (x) (if (null? x) (if #f 5 6) (if #t 7 8)))
         '()
         q))
  '(((lambda (_.0) (if (null? _.0) 6 7))
     (sym _.0))))

(test "nfo-if/pair?-0"
  (run* (q)
    (nfo '(lambda (x) (if (pair? x) (if #f 5 6) (if #t 7 8)))
         '()
         q))
  '(((lambda (_.0) (if (pair? _.0) 6 7)) (sym _.0))))


(test "nfo-car-1"
  (run* (q)
    (nfo '(car (cons 'cat 'dog)) '() q))
  '((quote cat)))

(test "nfo-car-2"
  (run* (q)
    (nfo '(car '(cat . dog)) '() q))
  '((quote cat)))

(test "nfo-car-3"
  (run* (q)
    (nfo '(lambda (x) (car '(cat . dog))) '() q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-4"
  (run* (q)
    (nfo '(lambda (x) (car (cons 'cat 'dog))) '() q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-5"
  (run* (q)
    (nfo '(lambda (x) (car (cons x 'dog))) '() q))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-car-6"
  (run* (q)
    (nfo '(lambda (x) (car (cons 'cat x))) '() q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-7"
  (run* (q)
    (nfo '(car (lambda (x) x)) '() q))
  '())


(test "nfo-cdr-1"
  (run* (q)
    (nfo '(cdr (cons 'cat 'dog)) '() q))
  '((quote dog)))

(test "nfo-cdr-2"
  (run* (q)
    (nfo '(cdr '(cat . dog)) '() q))
  '((quote dog)))

(test "nfo-cdr-3"
  (run* (q)
    (nfo '(lambda (x) (cdr '(cat . dog))) '() q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-4"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons 'cat 'dog))) '() q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-5"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons x 'dog))) '() q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-6"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons 'cat x))) '() q))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-cdr-7"
  (run* (q)
    (nfo '(cdr (lambda (x) x)) '() q))
  '())



(test "nfo-lambda-1"
  (run* (q)
    (nfo '(lambda (x) x) '() q))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "nfo-lambda-2"
  (run* (q)
    (nfo '(lambda (y) y) '() q))
  '(((lambda (_.0) _.0) (sym _.0))))

;; open terms are not allowed
;; TODO try experimenting with the '() to handle open terms
(test "nfo-lambda-3"
  (run* (q)
    (nfo '(lambda (x) y) '() q))
  '())

(test "nfo-lambda-4"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) x)) '() q))
  '(((lambda (_.0) (lambda (_.1) _.0))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-5"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) y)) '() q))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-6"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) (quote (x . y)))) '() q))
  '(((lambda (_.0) (lambda (_.1) '(x . y)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-7"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) (cons x y))) '() q))
  '(((lambda (_.0) (lambda (_.1) (cons _.0 _.1)))
     (=/= ((_.0 N)) ((_.0 _.1)) ((_.0 closure)) ((_.1 N))
          ((_.1 closure)))
     (sym _.0 _.1))))

(test "nfo-lambda-8"
  (run* (q)
    (nfo '(lambda (x) (cons x x)) '() q))
  '(((lambda (_.0) (cons _.0 _.0))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))

(test "nfo-lambda-9"
  (run* (q)
    (nfo '(lambda (x) (cons 'cat x)) '() q))
  '(((lambda (_.0) (cons 'cat _.0))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))

(test "nfo-lambda-10"
  (run* (q)
    (nfo '(lambda (x) (cons x 'cat)) '() q))
  '(((lambda (_.0) (cons _.0 'cat))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))

(test "nfo-lambda-11"
  (run* (q)
    (nfo '(lambda (x) (cons 'cat 'dog)) '() q))
  '(((lambda (_.0) '(cat . dog))
     (sym _.0))))

(test "nfo-lambda-12"
  (run* (q)
    (nfo '((lambda (x) x) (lambda (y) y)) '() q))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "nfo-lambda-13"
  (run* (q)
    (nfo '(lambda (w) ((lambda (x) x) (lambda (y) y))) '() q))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))


(test "nfo-cons-lambda-1"
  (run* (q)
    (nfo '(cons (lambda (x) x) (lambda (y) y)) '() q))
  '(((cons (lambda (_.0) _.0) (lambda (_.1) _.1))
     (=/= ((_.0 N)) ((_.0 closure)) ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))))

(test "nfo-cons-lambda-2"
  (run* (q)
    (nfo '(cons (lambda (x) x) (lambda (x) x)) '() q))
  '(((cons (lambda (_.0) _.0) (lambda (_.1) _.1))
     (=/= ((_.0 N)) ((_.0 closure)) ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))))

(test "nfo-cons-lambda-3"
  (run* (q)
    (nfo '(cons (lambda (x) (lambda (y) x))
                (lambda (x) (lambda (y) x)))
         '()
         q))
  '(((cons
       (lambda (_.0) (lambda (_.1) _.0))
       (lambda (_.2) (lambda (_.3) _.2)))
     (=/= ((_.0 N)) ((_.0 _.1)) ((_.0 closure)) ((_.1 N))
          ((_.1 closure)) ((_.2 N)) ((_.2 _.3)) ((_.2 closure))
          ((_.3 N)) ((_.3 closure)))
     (sym _.0 _.1 _.2 _.3))))

(test "nfo-cons-lambda-4"
  (run* (q)
    (nfo '(cons (lambda (x) (lambda (y) x))
                (lambda (x) (lambda (y) y)))
         '()
         q))
  '(((cons
       (lambda (_.0) (lambda (_.1) _.0))
       (lambda (_.2) (lambda (_.3) _.3)))
     (=/= ((_.0 N)) ((_.0 _.1)) ((_.0 closure)) ((_.1 N))
          ((_.1 closure)) ((_.2 N)) ((_.2 _.3)) ((_.2 closure))
          ((_.3 N)) ((_.3 closure)))
     (sym _.0 _.1 _.2 _.3))))


(test "eval-expro-lambda-6"
  (run* (q)
    (eval-expro '(lambda (x) (lambda (y) (quote (x . y)))) '() q))
  '((closure (x) (lambda (y) (quote (x . y))) ())))

(test "eval-expro-lambda-7"
  (run* (q)
    (eval-expro '(lambda (x) (lambda (y) (cons x y))) '() q))
  '((closure (x) (lambda (y) (cons x y)) ())))

(test "eval-expro-lambda-8"
  (run* (q)
    (eval-expro '(lambda (x) (cons x x)) '() q))
  '((closure (x) (cons x x) ())))


(test "uneval-valueo-lambda-6"
  (run* (q)
    (uneval-valueo '() '(closure (x) (lambda (y) (quote (x . y))) ()) q))
  '(((lambda (_.0) (lambda (_.1) (quote (x . y))))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "uneval-valueo-lambda-7"
  (run* (q)
    (uneval-valueo '() '(closure (x) (lambda (y) (cons x y)) ()) q))
  '(((lambda (_.0) (lambda (_.1) (cons _.0 _.1)))
     (=/= ((_.0 N)) ((_.0 _.1)) ((_.0 closure)) ((_.1 N))
          ((_.1 closure)))
     (sym _.0 _.1))))

(test "uneval-valueo-lambda-8"
  (run* (q)
    (uneval-valueo '() '(closure (x) (cons x x) ()) q))
  '(((lambda (_.0) (cons _.0 _.0))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))


(test "eval-expro-quote-1"
  (run* (val)
    (eval-expro `(quote ()) '() val))
  '(()))

(test "eval-expro-quote-2"
  (run* (val)
    (eval-expro `(quote cat) '() val))
  '(cat))

(test "eval-expro-quote-3"
  (run* (val)
    (eval-expro `(quote (foo bar baz)) '() val))
  '((foo bar baz)))

(test "eval-expro-quote-4"
  (run* (val)
    (eval-expro `(quote (quote () quote foo ((cat dog)))) '() val))
  '((quote () quote foo ((cat dog)))))

(test "eval-expro-quote-5"
  (run* (val)
    (eval-expro `(quote (foo bar . baz)) '() val))
  '((foo bar . baz)))


(test "eval-expro-cons-1"
  (run* (val)
    (eval-expro `(cons (quote cat) (quote dog)) '() val))
  '((cat . dog)))

(test "eval-expro-cons-2"
  (run* (val)
    (eval-expro `(cons (quote cat) (quote ())) '() val))
  '((cat)))

(test "eval-expro-cons-3"
  (run* (val)
    (eval-expro `(cons (quote cat) (cons (quote fox) (quote ()))) '() val))
  '((cat fox)))


(test "nfo-cons-1"
  (run* (expr)
    (nfo `(cons (quote cat) (cons (quote fox) (quote ()))) '() expr))
  '((quote (cat fox))))

(test "nfo-cons-2"
  (run* (expr)
    (nfo `(cons (quote cat) (quote fox)) '() expr))
  '((quote (cat . fox))))

(test "nfo-cons-3"
  (run* (expr)
    (nfo `(cons (cons (quote a) (quote b)) (cons (quote c) (quote d)))
         '()
         expr))
  '((quote ((a . b) . (c . d)))))


(test "nfo-quoted-pair-backwards-1"
  (run 13 (expr)
    (nfo expr
         '()
         '(quote ((a . b) . (c . d)))))
  '('((a . b) c . d)
    ((car '(((a . b) c . d) . _.0))
     (absento (N _.0) (closure _.0)))
    ((cdr '(_.0 (a . b) c . d))
     (absento (N _.0) (closure _.0)))
    (if #f _.0 '((a . b) c . d))
    (if #t '((a . b) c . d) _.0)
    (cons '(a . b) '(c . d))
    (if '#t '((a . b) c . d) _.0)
    ((car (car '((((a . b) c . d) . _.0) . _.1)))
     (absento (N _.0) (N _.1) (closure _.0) (closure _.1)))
    (if '#f _.0 '((a . b) c . d))
    (((lambda (_.0) '((a . b) c . d)) #f)
     (sym _.0))
    ((car (cdr '(_.0 ((a . b) c . d) . _.1)))
     (absento (N _.0) (N _.1) (closure _.0) (closure _.1)))
    ((cdr (car '((_.0 (a . b) c . d) . _.1)))
     (absento (N _.0) (N _.1) (closure _.0) (closure _.1)))
    (if (null? #f) _.0 '((a . b) c . d))))


(test "eval-expro-1"
  (run* (val)
    (eval-expro `(lambda (z) z) '() val))
  '((closure (z) z ())))

(test "eval-expro-2"
  (run* (val)
    (eval-expro
     `((lambda (x) (lambda (y) x))
       (lambda (z) z))
     '()
     val))
  '((closure (y) x ((x . (closure (z) z ()))))))

(test "eval-expro-3"
  (run* (val)
    (eval-expro
     `(((lambda (f)
          ((lambda (x) (f (lambda (v) ((x x) v))))
           (lambda (x) (f (lambda (v) ((x x) v))))))
        (lambda (list?)
          (lambda (l)
            (if (null? l)
                #t
                (if (pair? l)
                    (list? (cdr l))
                    #f)))))
       '(1 2 3))
     '()
     val))
  '(#t))

(test "eval-expro-4"
  (run* (val)
    (eval-expro
     `(((lambda (f)
          ((lambda (x) (f (lambda (v) ((x x) v))))
           (lambda (x) (f (lambda (v) ((x x) v))))))
        (lambda (list?)
          (lambda (l)
            (if (null? l)
                #t
                (if (pair? l)
                    (list? (cdr l))
                    #f)))))
       '(1 2 . 3))
     '()
     val))
  '(#f))

(test "eval-expro-5"
  (run* (val)
    (eval-expro
     `((lambda (Z)
         ((lambda (F)
            ((Z F) '(1 2 . 3)))
          (lambda (list?)
            (lambda (l)
              (if (null? l)
                  #t
                  (if (pair? l)
                      (list? (cdr l))
                      #f))))))
       (lambda (f)
         ((lambda (x) (f (lambda (v) ((x x) v))))
          (lambda (x) (f (lambda (v) ((x x) v)))))))
     '()
     val))
  '(#f))


#|
;; This test appears to diverge, presumably
;; due to the unevaling of self-application.
(test "eval-expro/uneval-valueo-ZF-1"
  (run 1 (val expr^)
    (eval-expro
     `((lambda (Z)
         ((lambda (F)
            (Z F))
          (lambda (list?)
            (lambda (l)
              (if (null? l)
                  #t
                  (if (pair? l)
                      (list? (cdr l))
                      #f))))))
       (lambda (f)
         ((lambda (x) (f (lambda (v) ((x x) v))))
          (lambda (x) (f (lambda (v) ((x x) v)))))))
     '()
     val)
    (uneval-valueo '() val expr^))
  '???)
|#

(test "uneval-valueo-0"
  (run 10 (val expr)
    (uneval-valueo '() val expr))
  '((#f #f)
    (#t #t)
    ((_.0 _.0)
     (num _.0))
    (() '())
    ((_.0 '_.0)
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))
    (((N (NVar _.0)) _.0)
     (sym _.0))
    (((N (NNull? (NVar _.0))) (null? _.0))
     (sym _.0))
    (((closure (_.0) #f _.1) (lambda (_.2) #f))
     (sym _.0 _.2))
    (((N (NPair? (NVar _.0))) (pair? _.0))
     (sym _.0))
    (((closure (_.0) #t _.1) (lambda (_.2) #t))
     (sym _.0 _.2))))

(test "uneval-valueo-1"
  (run* (expr)
    (uneval-valueo '() '(closure (y) x ((x . (closure (z) z ())))) expr))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-2"
  (run* (result)
    (fresh (val)
      (eval-expro
       `((lambda (x) (lambda (y) x))
         (lambda (z) z))
       '()
       val)
      (uneval-valueo '() val result)))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-3"
  (run* (expr)
    (nfo
     `(lambda (y)
        ((lambda (x) (lambda (y) x))
         y))
     '()
     expr))
  '(((lambda (_.0) (lambda (_.1) _.0))
     (=/= ((_.0 _.1))) (sym _.0 _.1))))

(test "eval-expro/uneval-valueo-4"
  (run* (q)
    (nfo
     `(lambda (y)
        ((lambda (x) (lambda (y) x))
         y))
     '()
     `(lambda (y) (lambda (y) y))))
  '())

(test "eval-expro/uneval-valueo-4-expressed"
  (run* (q)
    (nfo
     `(lambda (y)
        ((lambda (x) (lambda (y) x))
         y))
     '()
     `(lambda (y) (lambda (z) y))))
  '(_.0))
