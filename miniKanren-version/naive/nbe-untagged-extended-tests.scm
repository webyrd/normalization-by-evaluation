(load "nbe-untagged-extended.scm")
(load "../faster-miniKanren/test-check.scm")

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
  (run 1 (q)
    (eval-expro q '() q))
  '((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))))

(test "nfo-quine-1"
  (run 10 (q)
    (nfo q '() q))
  '(('_.0 (=/= ((_.0 N)) ((_.0 closure))) (sym _.0))
    '()
    ('(_.0 . _.1)
     (=/= ((_.0 N)) ((_.0 closure)) ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))
    ('(_.0) (=/= ((_.0 N)) ((_.0 closure))) (sym _.0))
    ('(() . _.0) (=/= ((_.0 N)) ((_.0 closure))) (sym _.0))
    '(())
    ((lambda (_.0) '_.1)
     (=/= ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))
    ((lambda (_.0) '()) (sym _.0))
    ('(_.0 _.1 . _.2)
     (=/= ((_.0 N)) ((_.0 closure))
          ((_.1 N)) ((_.1 closure))
          ((_.2 N)) ((_.2 closure)))
     (sym _.0 _.1 _.2))
    ('(_.0 _.1)
     (=/= ((_.0 N)) ((_.0 closure)) ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))))


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
  '('((a . b) . (c . d))
    (cons '(a . b) '(c . d))
    (((lambda (_.0) '((a . b) . (c . d))) '_.1)
     (sym _.0)
     (absento (N _.1) (closure _.1)))
    (cons '(a . b) (cons 'c 'd))
    (((lambda (_.0) _.0) '((a . b) . (c . d)))
     (sym _.0))
    (cons (cons 'a 'b) '(c . d))
    (((lambda (_.0) (cons '(a . b) '(c . d))) '_.1)
     (sym _.0)
     (absento (N _.1) (closure _.1)))
    (((lambda (_.0) '((a . b) . (c . d))) (lambda (_.1) _.2))
     (sym _.0 _.1))
    ((cons '(a . b) ((lambda (_.0) '(c . d)) '_.1))
     (sym _.0)
     (absento (N _.1) (closure _.1)))
    (((lambda (_.0) (cons '(a . b) _.0)) '(c . d)) (sym _.0))
    (((lambda (_.0) (cons '(a . b) '(c . d)))
      (lambda (_.1) _.2))
     (sym _.0 _.1))
    (((lambda (_.0) ((lambda (_.1) '((a . b) . (c . d))) '_.2))
      '_.3)
     (sym _.0 _.1)
     (absento (N _.2) (N _.3) (closure _.2) (closure _.3)))
    (cons (cons 'a 'b) (cons 'c 'd))))


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

(test "uneval-valueo-0"
  (run 8 (val expr)
    (uneval-valueo '() val expr))
  '(((_.0 '_.0)
     (=/= ((_.0 N)) ((_.0 closure)))
     (sym _.0))
    (() '())
    (((N (NVar _.0)) _.0)
     (sym _.0))
    (((N (NApp (NVar _.0) _.1)) (_.0 '_.1))
     (=/= ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))
    (((N (NApp (NVar _.0) ())) (_.0 '()))
     (sym _.0))
    (((closure (_.0) '_.1 _.2) (lambda (_.3) '_.1))
     (=/= ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1 _.3))
    (((closure (_.0) '() _.1) (lambda (_.2) '()))
     (sym _.0 _.2))
    (((_.0 . _.1) '(_.0 . _.1))
     (=/= ((_.0 N)) ((_.0 closure)) ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))))

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
