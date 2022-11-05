(load "nbe-untagged-extended.scm")
(load "../faster-miniKanren/test-check.scm")

(test "main"
  (run* (result)
    (fresh (id_ const_)
      (eval-expro '(lambda (x) x) '() id_)
      (eval-expro '(lambda (x) (lambda (y) x)) '() const_)
      (eval-expro '(const id) `((id . ,id_) (const . ,const_)) result)))
  '((closure (y) x ((x . (closure (x) x ()))))))


(test "quine-1"
  (run 1 (q)
    (eval-expro q '() q))
  '((((lambda (_.0)
        (cons _.0 (cons (cons 'quote (cons _.0 '())) '())))
      '(lambda (_.0)
         (cons _.0 (cons (cons 'quote (cons _.0 '())) '()))))
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
    (((_.0 . _.1) '(_.0 . _.1))
     (absento (N _.0) (N _.1) (closure _.0) (closure _.1)))
    (((N (NVar _.0)) _.0)
     (sym _.0))
    (((closure (_.0) '_.1 _.2) (lambda (_.3) '_.1))
     (=/= ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1 _.3))
    (((N (NApp (NVar _.0) _.1)) (_.0 '_.1))
     (=/= ((_.1 N)) ((_.1 closure)))
     (sym _.0 _.1))
    (((closure (_.0) '() _.1) (lambda (_.2) '()))
     (sym _.0 _.2))
    (((N (NApp (NVar _.0) ())) (_.0 '()))
     (sym _.0))))

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
