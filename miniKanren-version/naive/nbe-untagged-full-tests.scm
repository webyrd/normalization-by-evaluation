(load "nbe-untagged-full.scm")
(load "../faster-miniKanren/test-check.scm")

(test "nfo-I-love-you"
  (run 99 (expr)
    (nfo expr '(quote (I love you))))
  '('(I love you)
    (letrec ()
      '(I love you))
    (let ()
      '(I love you))
    (let* ()
      '(I love you))
    ((letrec ([_.0 (lambda _.1 _.2)])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda () _.1)])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((let ([_.0 _.1])
       '(I love you))
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0))
    (and '(I love you))
    ((letrec ([_.0 (lambda (_.1) _.2)])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 _.2)] [_.3 (lambda _.4 _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3 _.4))
    (or '(I love you))
    ((let ([_.0 list])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((if _.0 '(I love you) _.1)
     (num _.0))
    ((let ([_.0 _.1] [_.2 _.3])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)))
     (num _.1 _.3)
     (sym _.0 _.2))
    (letrec ()
      (letrec ()
        '(I love you)))
    (or '(I love you) _.0 . _.1)
    ((and _.0 '(I love you))
     (num _.0))
    ((let* ([_.0 '(I love you)])
       _.0)
     (sym _.0))
    ((letrec ([_.0 (lambda (_.1 _.2) _.3)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)))
     (sym _.0 _.1 _.2))
    ((letrec ([_.0 (lambda _.1 _.2)] [_.3 (lambda () _.4)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3))
    ((letrec ([_.0 (lambda () _.1)] [_.2 (lambda _.3 _.4)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)))
     (sym _.0 _.2 _.3))
    (if list '(I love you) _.0)
    ((let ([_.0 not])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0))
    (let ()
      (letrec ()
        '(I love you)))
    (letrec ()
      (let ()
        '(I love you)))
    ((let ([_.0 list] [_.1 _.2])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (num _.2)
     (sym _.0 _.1))
    ((let ([_.0 _.1] [_.2 _.3] [_.4 _.5])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.4 quote)))
     (num _.1 _.3 _.5)
     (sym _.0 _.2 _.4))
    ((letrec ([_.0 (lambda _.1 '(I love you))])
       (_.0))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (sym _.0 _.1))
    ((let ([_.0 '(I love you)])
       _.0)
     (sym _.0))
    (and list '(I love you))
    ((letrec ([_.0 (lambda () _.1)] [_.2 (lambda () _.3)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)))
     (sym _.0 _.2))
    ((letrec ([_.0 (lambda (_.1) _.2)] [_.3 (lambda _.4 _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3 _.4))
    ((letrec ([_.0 (lambda _.1 _.2)] [_.3 (lambda (_.4) _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3 _.4))
    ((letrec ([_.0 (lambda (_.1 _.2 _.3) _.4)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)) ((_.1 _.3)) ((_.2 _.3)))
     (sym _.0 _.1 _.2 _.3))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda _.4 _.5)]
              [_.6 (lambda _.7 _.8)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.6 quote)))
     (sym _.0 _.1 _.3 _.4 _.6 _.7))
    (if not '(I love you) _.0)
    (let ()
      (let ()
        '(I love you)))
    ((let ([_.0 equal?])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((letrec ()
       (letrec ([_.0 (lambda _.1 _.2)])
         '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    (letrec ()
      (let* ()
        '(I love you)))
    ((let ([_.0 _.1] [_.2 list])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)))
     (num _.1)
     (sym _.0 _.2))
    (or '#f '(I love you))
    (let* ()
      (letrec ()
        '(I love you)))
    ((letrec ([_.0 (lambda _.1 _.2)])
       (letrec ()
         '(I love you)))
     (=/= ((_.0 letrec)) ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 '(I love you))])
       (_.0 _.2))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (num _.2)
     (sym _.0 _.1))
    ((let ([_.0 _.1] [_.2 _.3] [_.4 list])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.4 quote)))
     (num _.1 _.3)
     (sym _.0 _.2 _.4))
    ((letrec ([_.0 (lambda () '(I love you))])
       (_.0))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((let ([_.0 _.1] [_.2 _.3] [_.4 _.5] [_.6 _.7])
       '(I love you))
     (=/= ((_.0 quote))
          ((_.2 quote))
          ((_.4 quote))
          ((_.6 quote)))
     (num _.1 _.3 _.5 _.7)
     (sym _.0 _.2 _.4 _.6))
    ((and _.0 _.1 '(I love you)) (num _.0 _.1))
    (and not '(I love you))
    (or '#f '(I love you) _.0 . _.1)
    ((letrec ([_.0 (lambda (_.1) _.2)] [_.3 (lambda () _.4)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3))
    ((letrec ([_.0 (lambda () _.1)] [_.2 (lambda (_.3) _.4)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)))
     (sym _.0 _.2 _.3))
    ((letrec ([_.0 (lambda (_.1 _.2) _.3)]
              [_.4 (lambda _.5 _.6)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)) ((_.4 quote)))
     (sym _.0 _.1 _.2 _.4 _.5))
    (let ()
      (let* ()
        '(I love you)))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda (_.4 _.5) _.6)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.4 _.5)))
     (sym _.0 _.1 _.3 _.4 _.5))
    ((let ()
       (letrec ([_.0 (lambda _.1 _.2)])
         '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda _.4 _.5)]
              [_.6 (lambda () _.7)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.6 quote)))
     (sym _.0 _.1 _.3 _.4 _.6))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda () _.4)]
              [_.5 (lambda _.6 _.7)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.5 quote)))
     (sym _.0 _.1 _.3 _.5 _.6))
    ((letrec ([_.0 (lambda () _.1)]
              [_.2 (lambda _.3 _.4)]
              [_.5 (lambda _.6 _.7)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.5 quote)))
     (sym _.0 _.2 _.3 _.5 _.6))
    ((letrec ([_.0 (lambda (_.1 _.2 _.3 _.4) _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)) ((_.1 _.3)) ((_.1 _.4))
          ((_.2 _.3)) ((_.2 _.4)) ((_.3 _.4)))
     (sym _.0 _.1 _.2 _.3 _.4))
    ((let ([_.0 '_.1])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0)
     (absento (closure _.1) (neutral _.1) (primitive _.1)))
    (if equal? '(I love you) _.0)
    (let* ()
      (let ()
        '(I love you)))
    ((letrec ()
       (letrec ([_.0 (lambda () _.1)])
         '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((letrec ()
       (let ([_.0 _.1])
         '(I love you)))
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0))
    (letrec ()
      (and '(I love you)))
    (and (letrec ()
           '(I love you)))
    ((let ([_.0 _.1])
       (letrec ()
         '(I love you)))
     (=/= ((_.0 letrec)) ((_.0 quote)))
     (num _.1)
     (sym _.0))
    ((let ([_.0 not] [_.1 _.2])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (num _.2)
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 _.2)])
       (let ()
         '(I love you)))
     (=/= ((_.0 let)) ((_.0 quote)))
     (sym _.0 _.1))
    ((let ([_.0 list] [_.1 _.2] [_.3 _.4])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 quote)) ((_.3 quote)))
     (num _.2 _.4)
     (sym _.0 _.1 _.3))
    ((letrec ([_.0 (lambda () _.1)])
       (letrec ()
         '(I love you)))
     (=/= ((_.0 letrec)) ((_.0 quote)))
     (sym _.0))
    ((letrec ([_.0 (lambda _.1 '(I love you))])
       (_.0 _.2 _.3))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (num _.2 _.3)
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda _.4 '(I love you))])
       (_.3))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.4 quote)))
     (sym _.0 _.1 _.3 _.4))
    ((and list _.0 '(I love you))
     (num _.0))
    (or #f '(I love you))
    ((and _.0 list '(I love you))
     (num _.0))
    ((letrec ([_.0 (lambda (_.1) '(I love you))])
       (_.0 _.2))
     (=/= ((_.0 quote)) ((_.1 quote)))
     (num _.2)
     (sym _.0 _.1))
    (and equal? '(I love you))
    ((let ()
       (letrec ([_.0 (lambda () _.1)])
         '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((let ()
       (let ([_.0 _.1])
         '(I love you)))
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0))
    ((let ([_.0 _.1] [_.2 _.3] [_.4 _.5] [_.6 list])
       '(I love you))
     (=/= ((_.0 quote))
          ((_.2 quote))
          ((_.4 quote))
          ((_.6 quote)))
     (num _.1 _.3 _.5)
     (sym _.0 _.2 _.4 _.6))
    ((letrec ([_.0 (lambda (_.1 _.2) _.3)]
              [_.4 (lambda () _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)) ((_.4 quote)))
     (sym _.0 _.1 _.2 _.4))
    ((letrec ([_.0 (lambda (_.1) _.2)] [_.3 (lambda (_.4) _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3 _.4))
    (let* ()
      (let* ()
        '(I love you)))
    ((letrec ([_.0 (lambda (_.1 _.2 _.3) _.4)]
              [_.5 (lambda _.6 _.7)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.1 _.2)) ((_.1 _.3)) ((_.2 _.3))
          ((_.5 quote)))
     (sym _.0 _.1 _.2 _.3 _.5 _.6))
    ((letrec ([_.0 (lambda () _.1)]
              [_.2 (lambda (_.3 _.4) _.5)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.3 _.4)))
     (sym _.0 _.2 _.3 _.4))
    ((let* ()
       (letrec ([_.0 (lambda _.1 _.2)])
         '(I love you)))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((let ([_.0 symbol?])
       '(I love you))
     (=/= ((_.0 quote)))
     (sym _.0))
    (let ()
      (and '(I love you)))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda () _.4)]
              [_.5 (lambda () _.6)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.5 quote)))
     (sym _.0 _.1 _.3 _.5))
    ((letrec ([_.0 (lambda () _.1)]
              [_.2 (lambda _.3 _.4)]
              [_.5 (lambda () _.6)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.5 quote)))
     (sym _.0 _.2 _.3 _.5))
    ((letrec ([_.0 (lambda () _.1)]
              [_.2 (lambda () _.3)]
              [_.4 (lambda _.5 _.6)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.4 quote)))
     (sym _.0 _.2 _.4 _.5))
    ((letrec ([_.0 (lambda (_.1) _.2)]
              [_.3 (lambda _.4 _.5)]
              [_.6 (lambda _.7 _.8)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.6 quote)))
     (sym _.0 _.1 _.3 _.4 _.6 _.7))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda (_.4) _.5)]
              [_.6 (lambda _.7 _.8)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.6 quote)))
     (sym _.0 _.1 _.3 _.4 _.6 _.7))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda _.4 _.5)]
              [_.6 (lambda (_.7) _.8)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.6 quote)))
     (sym _.0 _.1 _.3 _.4 _.6 _.7))
    ((letrec ([_.0 (lambda _.1 _.2)]
              [_.3 (lambda (_.4 _.5 _.6) _.7)])
       '(I love you))
     (=/= ((_.0 quote)) ((_.3 quote)) ((_.4 _.5)) ((_.4 _.6))
          ((_.5 _.6)))
     (sym _.0 _.1 _.3 _.4 _.5 _.6))
    ((let ([_.0 _.1] [_.2 _.3] [_.4 _.5] [_.6 _.7] [_.8 _.9])
       '(I love you))
     (=/= ((_.0 quote)) ((_.2 quote)) ((_.4 quote)) ((_.6 quote))
          ((_.8 quote)))
     (num _.1 _.3 _.5 _.7 _.9)
     (sym _.0 _.2 _.4 _.6 _.8))))



(test "foo-1"
  (run* (expr)
    (nfo '(lambda (x) ((lambda (y) y) 5))
         expr))
  '(((lambda (_.0) 5) (sym _.0))))

(test "foo-2"
  (run* (val)
    (evalo '(lambda (x) ((lambda (y) y) 5))
           val))
  '((closure
      (lambda (x) ((lambda (y) y) 5))
      ((val list closure (lambda x x) ())
       (val not primitive . not)
       (val equal? primitive . equal?)
       (val symbol? primitive . symbol?)
       (val number? primitive . number?)
       (val cons primitive . cons)
       (val null? primitive . null?)
       (val pair? primitive . pair?)
       (val car primitive . car)
       (val cdr primitive . cdr)
       (val procedure? primitive . procedure?)))))

(test "foo-3"
  (run 3 (env val)
    (eval-expro '(lambda (x) ((lambda (y) y) 5))
                env
                val))
  '((()
     (closure (lambda (x) ((lambda (y) y) 5)) ()))
    ((((val _.0 . _.1))
      (closure (lambda (x) ((lambda (y) y) 5)) ((val _.0 . _.1))))
     (=/= ((_.0 lambda))))
    (((rec))
     (closure (lambda (x) ((lambda (y) y) 5)) ((rec))))))

(test "foo-4"
  (run* (val)
    (evalo '((lambda (x) (lambda (y) x)) 5)
           val))
  '((closure
      (lambda (y) x)
      ((val x . 5)
       (val list closure (lambda x x) ())
       (val not primitive . not)
       (val equal? primitive . equal?)
       (val symbol? primitive . symbol?)
       (val number? primitive . number?)
       (val cons primitive . cons)
       (val null? primitive . null?)
       (val pair? primitive . pair?)
       (val car primitive . car)
       (val cdr primitive . cdr)
       (val procedure? primitive . procedure?)))))

(test "foo-5"
  (run 3 (env val)
    (eval-expro '(cons x 5)
                env
                val))
  '((((val cons neutral _.0) (val x . _.1) . _.2)
     (neutral (NApp _.0 (_.1 5))))
    ((((val cons primitive . cons) (val x . _.0) . _.1)
      (_.0 . 5))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive))))
    ((((val cons closure (lambda _.0 _.1) _.2)
       (val x . _.3)
       .
       _.4)
      _.1)
     (num _.1)
     (sym _.0))))

(test "foo-6"
  ;; cons doesn't show this behavior, since it is a constructor not a destructor
  (run 2 (env val)
    (eval-expro '(car x)
                env
                val))
  '((((val car neutral _.0) (val x . _.1) . _.2)
     (neutral (NApp _.0 (_.1))))
    ((((val car closure (lambda _.0 _.1) _.2) (val x . _.3) . _.4) _.1)
     (num _.1)
     (sym _.0))))

(test "foo-7"
  (run 2 (env val)
    (eval-expro '(x 1)
                env
                val))
  '((((val x neutral _.0) . _.1)
     (neutral (NApp _.0 (1))))
    ((((val x closure (lambda _.0 _.1) _.2) . _.3) _.1)
     (num _.1)
     (sym _.0))))




(test "nfo-#f-1"
  (run* (q)
    (nfo '#f q))
  '(#f))

(test "nfo-#f-2"
  (run* (q)
    (nfo '(quote #f) q))
  '(#f))

(test "nfo-#f-3"
  (run* (q)
    (nfo '((lambda (x) x) (quote #f)) q))
  '(#f))


(test "nfo-#t-1"
  (run* (q)
    (nfo '#t q))
  '(#t))

(test "nfo-#t-2"
  (run* (q)
    (nfo '(quote #t) q))
  '(#t))

(test "nfo-#t-3"
  (run* (q)
    (nfo '((lambda (x) x) (quote #t)) q))
  '(#t))


(test "nfo-number-1"
  (run* (q)
    (nfo '5 q))
  '(5))

(test "nfo-number-2"
  (run* (q)
    (nfo '(quote 5) q))
  '(5))

(test "nfo-number-3"
  (run* (q)
    (nfo '(lambda (x) (quote 5)) q))
  '(((lambda (_.0) 5)
     (sym _.0))))

(test "nfo-number-4"
  (run* (q)
    (nfo '(cons 5 6) q))
  '((quote (5 . 6))))

(test "nfo-number-5"
  (run* (q)
    (nfo '(cons 5 #f) q))
  '((quote (5 . #f))))

(test "nfo-number-6"
  (run* (q)
    (nfo '(cons #f 5) q))
  '((quote (#f . 5))))

(test "nfo-number-7"
  (run* (q)
    (nfo '(cons (quote #f) 5) q))
  '((quote (#f . 5))))

(test "nfo-number-8"
  (run* (q)
    (nfo '(cons (quote #f) (quote 5)) q))
  '((quote (#f . 5))))

(test "nfo-number-9"
  (run* (q)
    (nfo '(cons #f (quote 5)) q))
  '((quote (#f . 5))))

(test "nfo-number-10"
  (run* (q)
    (nfo '(lambda (x) (cons 5 (quote 5))) q))
  '(((lambda (_.0) '(5 . 5))
     (sym _.0))))

(test "nfo-number-11"
  (run* (q)
    (nfo '(lambda (x) (cons 5 #f)) q))
  '(((lambda (_.0) '(5 . #f))
     (sym _.0))))

(test "nfo-number-12"
  (run* (q)
    (nfo '(lambda (x) (cons 5 (quote ()))) q))
  '(((lambda (_.0) '(5 . ()))
     (sym _.0))))


(test "nfo-null?-0"
  (run* (q)
    (nfo '(null? (quote ())) q))
  '(#t))

(test "nfo-null?-1"
  (run* (q)
    (nfo '(null? 5) q))
  '(#f))

(test "nfo-null?-2"
  (run* (q)
    (nfo '(null? #t) q))
  '(#f))

(test "nfo-null?-3"
  (run* (q)
    (nfo '(null? (lambda (x) x)) q))
  '(#f))

(test "nfo-null?-4"
  (run* (q)
    (nfo '(null? (cons 3 4)) q))
  '(#f))

(test "evalo-null?-4"
  (run* (q)
    (evalo '(null? (cons 3 4)) q))
  '(#f))

(test "nfo-null?-4b"
  (run* (q)
    (nfo '(null? (cons (cons 6 7) (cons 4 5))) q))
  '(#f))

(test "evalo-null?-4b"
  (run* (q)
    (evalo '(null? (cons (cons 6 7) (cons 4 5))) q))
  '(#f))

(test "nfo-null?-5"
  (run* (q)
    (nfo '(null? 'cat) q))
  '(#f))


(test "nfo-pair?-0"
  (run* (q)
    (nfo '(pair? (quote ())) q))
  '(#f))

(test "nfo-pair?-1"
  (run* (q)
    (nfo '(pair? 5) q))
  '(#f))

(test "nfo-pair?-2"
  (run* (q)
    (nfo '(pair? #t) q))
  '(#f))

(test "nfo-pair?-3"
  (run* (q)
    (nfo '(pair? (lambda (x) x)) q))
  '(#f))

(test "nfo-pair?-4"
  (run* (q)
    (nfo '(pair? (cons 3 4)) q))
  '(#t))

(test "nfo-pair?-5"
  (run* (q)
    (nfo '(pair? 'cat) q))
  '(#f))


(test "nfo-number?-0"
  (run* (q)
    (nfo '(number? (quote ())) q))
  '(#f))

(test "nfo-number?-1"
  (run* (q)
    (nfo '(number? 5) q))
  '(#t))

(test "nfo-number?-2"
  (run* (q)
    (nfo '(number? #t) q))
  '(#f))

(test "nfo-number?-3"
  (run* (q)
    (nfo '(number? (lambda (x) x)) q))
  '(#f))

(test "nfo-number?-4"
  (run* (q)
    (nfo '(number? (cons 3 4)) q))
  '(#f))

(test "nfo-number?-5"
  (run* (q)
    (nfo '(number? 'cat) q))
  '(#f))


(test "nfo-symbol?-0"
  (run* (q)
    (nfo '(symbol? (quote ())) q))
  '(#f))

(test "nfo-symbol?-1"
  (run* (q)
    (nfo '(symbol? 5) q))
  '(#f))

(test "nfo-symbol?-2"
  (run* (q)
    (nfo '(symbol? #t) q))
  '(#f))

(test "nfo-symbol?-3"
  (run* (q)
    (nfo '(symbol? (lambda (x) x)) q))
  '(#f))

(test "nfo-symbol?-4"
  (run* (q)
    (nfo '(symbol? (cons 3 4)) q))
  '(#f))

(test "nfo-symbol?-5"
  (run* (q)
    (nfo '(symbol? 'cat) q))
  '(#t))


(test "nfo-if-0"
  (run* (q)
    (nfo '(if #f 5 6) q))
  '(6))

(test "nfo-if-1"
  (run* (q)
    (nfo '(if #t 5 6) q))
  '(5))

(test "nfo-if-2"
  (run* (q)
    (nfo '(if (if #t #f #t) 5 6) q))
  '(6))

(test "nfo-if-3"
  (run* (q)
    (nfo '(if (if #f #f #t) 5 6) q))
  '(5))

(test "nfo-if-4"
  (run* (q)
    (nfo '(if (if #f #f #t) (if #f 5 6) (if #t 7 8)) q))
  '(6))

(test "nfo-if-5"
  (run* (q)
    (nfo '(if (if #t #f #t) (if #f 5 6) (if #t 7 8)) q))
  '(7))

(test "nfo-if-6"
  (run* (q)
    (nfo '(lambda (x) (if x (if #f 5 6) (if #t 7 8))) q))
  '(((lambda (_.0) (if _.0 6 7))
     (sym _.0))))

(test "nfo-if/null?-0"
  (run* (q)
    (nfo '(lambda (x) (if (null? x) (if #f 5 6) (if #t 7 8)))
         q))
  '(((lambda (_.0) (if (null? _.0) 6 7))
     (sym _.0))))

(test "nfo-if/pair?-0"
  (run* (q)
    (nfo '(lambda (x) (if (pair? x) (if #f 5 6) (if #t 7 8)))
         q))
  '(((lambda (_.0) (if (pair? _.0) 6 7)) (sym _.0))))


(test "nfo-car-1"
  (run* (q)
    (nfo '(car (cons 'cat 'dog)) q))
  '((quote cat)))

(test "nfo-car-2"
  (run* (q)
    (nfo '(car '(cat . dog)) q))
  '((quote cat)))

(test "nfo-car-3"
  (run* (q)
    (nfo '(lambda (x) (car '(cat . dog))) q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-4"
  (run* (q)
    (nfo '(lambda (x) (car (cons 'cat 'dog))) q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-5"
  (run* (q)
    (nfo '(lambda (x) (car (cons x 'dog))) q))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-car-6"
  (run* (q)
    (nfo '(lambda (x) (car (cons 'cat x))) q))
  '(((lambda (_.0) (quote cat))
     (sym _.0))))

(test "nfo-car-7"
  (run* (q)
    (nfo '(car (lambda (x) x)) q))
  '())


(test "nfo-cdr-1"
  (run* (q)
    (nfo '(cdr (cons 'cat 'dog)) q))
  '((quote dog)))

(test "nfo-cdr-2"
  (run* (q)
    (nfo '(cdr '(cat . dog)) q))
  '((quote dog)))

(test "nfo-cdr-3"
  (run* (q)
    (nfo '(lambda (x) (cdr '(cat . dog))) q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-4"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons 'cat 'dog))) q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-5"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons x 'dog))) q))
  '(((lambda (_.0) (quote dog))
     (sym _.0))))

(test "nfo-cdr-6"
  (run* (q)
    (nfo '(lambda (x) (cdr (cons 'cat x))) q))
  '(((lambda (_.0) _.0)
     (sym _.0))))

(test "nfo-cdr-7"
  (run* (q)
    (nfo '(cdr (lambda (x) x)) q))
  '())



(test "nfo-lambda-1"
  (run* (q)
    (nfo '(lambda (x) x) q))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "nfo-lambda-2"
  (run* (q)
    (nfo '(lambda (y) y) q))
  '(((lambda (_.0) _.0) (sym _.0))))

;; open terms are not allowed
;; TODO try experimenting with the '() to handle open terms
(test "nfo-lambda-3"
  (run* (q)
    (nfo '(lambda (x) y) q))
  '())

(test "nfo-lambda-4"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) x)) q))
  '(((lambda (_.0) (lambda (_.1) _.0))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-5"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) y)) q))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-6"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) (quote (x . y)))) q))
  '(((lambda (_.0) (lambda (_.1) '(x . y)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))

(test "nfo-lambda-7"
  (run* (q)
    (nfo '(lambda (x) (lambda (y) (cons x y))) q))
  '(((lambda (_.0) (lambda (_.1) (cons _.0 _.1)))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 neutral))
          ((_.0 primitive)) ((_.1 closure)) ((_.1 neutral))
          ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-lambda-8"
  (run* (q)
    (nfo '(lambda (x) (cons x x)) q))
  '(((lambda (_.0) (cons _.0 _.0))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive)))
     (sym _.0))))

(test "nfo-lambda-9"
  (run* (q)
    (nfo '(lambda (x) (cons 'cat x)) q))
  '(((lambda (_.0) (cons 'cat _.0))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive)))
     (sym _.0))))

(test "nfo-lambda-10"
  (run* (q)
    (nfo '(lambda (x) (cons x 'cat)) q))
  '(((lambda (_.0) (cons _.0 'cat))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive)))
     (sym _.0))))

(test "nfo-lambda-11"
  (run* (q)
    (nfo '(lambda (x) (cons 'cat 'dog)) q))
  '(((lambda (_.0) '(cat . dog))
     (sym _.0))))

(test "nfo-lambda-12"
  (run* (q)
    (nfo '((lambda (x) x) (lambda (y) y)) q))
  '(((lambda (_.0) _.0) (sym _.0))))

(test "nfo-lambda-13"
  (run* (q)
    (nfo '(lambda (w) ((lambda (x) x) (lambda (y) y))) q))
  '(((lambda (_.0) (lambda (_.1) _.1))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))))


(test "nfo-cons-lambda-1"
  (run* (q)
    (nfo '(cons (lambda (x) x) (lambda (y) y)) q))
  '(((cons (lambda (_.0) _.0) (lambda (_.1) _.1))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive))
          ((_.1 closure)) ((_.1 neutral)) ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-cons-lambda-2"
  (run* (q)
    (nfo '(cons (lambda (x) x) (lambda (x) x)) q))
  '(((cons (lambda (_.0) _.0) (lambda (_.1) _.1))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive))
          ((_.1 closure)) ((_.1 neutral)) ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-cons-lambda-3"
  (run* (q)
    (nfo '(cons (lambda (x) (lambda (y) x))
                (lambda (x) (lambda (y) x)))
         q))
  '(((cons
       (lambda (_.0) (lambda (_.1) _.0))
       (lambda (_.2) (lambda (_.3) _.2)))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 neutral))
          ((_.0 primitive)) ((_.1 closure)) ((_.1 neutral))
          ((_.1 primitive)) ((_.2 _.3)) ((_.2 closure))
          ((_.2 neutral)) ((_.2 primitive)) ((_.3 closure))
          ((_.3 neutral)) ((_.3 primitive)))
     (sym _.0 _.1 _.2 _.3))))

(test "nfo-cons-lambda-4"
  (run* (q)
    (nfo '(cons (lambda (x) (lambda (y) x))
                (lambda (x) (lambda (y) y)))
         q))
  '(((cons
       (lambda (_.0) (lambda (_.1) _.0))
       (lambda (_.2) (lambda (_.3) _.3)))
     (=/= ((_.0 _.1)) ((_.0 closure)) ((_.0 neutral))
          ((_.0 primitive)) ((_.1 closure)) ((_.1 neutral))
          ((_.1 primitive)) ((_.2 _.3)) ((_.2 closure))
          ((_.2 neutral)) ((_.2 primitive)) ((_.3 closure))
          ((_.3 neutral)) ((_.3 primitive)))
     (sym _.0 _.1 _.2 _.3))))



(test "evalo-quote-1"
  (run* (val)
    (evalo `(quote ()) val))
  '(()))

(test "evalo-quote-2"
  (run* (val)
    (evalo `(quote cat) val))
  '(cat))

(test "evalo-quote-3"
  (run* (val)
    (evalo `(quote (foo bar baz)) val))
  '((foo bar baz)))

(test "evalo-quote-4"
  (run* (val)
    (evalo `(quote (quote () quote foo ((cat dog)))) val))
  '((quote () quote foo ((cat dog)))))

(test "evalo-quote-5"
  (run* (val)
    (evalo `(quote (foo bar . baz)) val))
  '((foo bar . baz)))


(test "evalo-cons-1"
  (run* (val)
    (evalo `(cons (quote cat) (quote dog)) val))
  '((cat . dog)))

(test "evalo-cons-2"
  (run* (val)
    (evalo `(cons (quote cat) (quote ())) val))
  '((cat)))

(test "evalo-cons-3"
  (run* (val)
    (evalo `(cons (quote cat) (cons (quote fox) (quote ()))) val))
  '((cat fox)))


(test "nfo-cons-1"
  (run* (expr)
    (nfo `(cons (quote cat) (cons (quote fox) (quote ()))) expr))
  '((quote (cat fox))))

(test "nfo-cons-2"
  (run* (expr)
    (nfo `(cons (quote cat) (quote fox)) expr))
  '((quote (cat . fox))))

(test "nfo-cons-3"
  (run* (expr)
    (nfo `(cons (cons (quote a) (quote b)) (cons (quote c) (quote d)))
         expr))
  '((quote ((a . b) . (c . d)))))


(test "nfo-quoted-pair-backwards-1"
  (run 13 (expr)
    (nfo expr
         '(quote ((a . b) . (c . d)))))
  '('((a . b) c . d)
    (letrec ()
      '((a . b) c . d))
    (let ()
      '((a . b) c . d))
    (let* ()
      '((a . b) c . d))
    ((letrec ([_.0 (lambda _.1 _.2)])
       '((a . b) c . d))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda () _.1)])
       '((a . b) c . d))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((let ([_.0 _.1])
       '((a . b) c . d))
     (=/= ((_.0 quote)))
     (num _.1)
     (sym _.0))
    (and '((a . b) c . d))
    ((letrec ([_.0 (lambda (_.1) _.2)])
       '((a . b) c . d))
     (=/= ((_.0 quote)))
     (sym _.0 _.1))
    ((letrec ([_.0 (lambda _.1 _.2)] [_.3 (lambda _.4 _.5)])
       '((a . b) c . d))
     (=/= ((_.0 quote)) ((_.3 quote)))
     (sym _.0 _.1 _.3 _.4))
    (or '((a . b) c . d))
    ((let ([_.0 list]) '((a . b) c . d))
     (=/= ((_.0 quote)))
     (sym _.0))
    ((if _.0 '((a . b) c . d) _.1)
     (num _.0))))


(test "evalo-1"
  (run* (val)
    (evalo `(lambda (z) z) val))
  '((closure
     (lambda (z) z)
     ((val list closure (lambda x x) ())
      (val not primitive . not)
      (val equal? primitive . equal?)
      (val symbol? primitive . symbol?)
      (val number? primitive . number?)
      (val cons primitive . cons) (val null? primitive . null?)
      (val pair? primitive . pair?) (val car primitive . car)
      (val cdr primitive . cdr)
      (val procedure? primitive . procedure?)))))

(test "evalo-2"
  (run* (val)
    (evalo
     `((lambda (x) (lambda (y) x))
       (lambda (z) z))
     val))
  '((closure
     (lambda (y) x)
     ((val x
           closure
           (lambda (z) z)
           ((val list closure (lambda x x) ()) (val not primitive . not) (val equal? primitive . equal?)
            (val symbol? primitive . symbol?)
            (val number? primitive . number?)
            (val cons primitive . cons) (val null? primitive . null?)
            (val pair? primitive . pair?) (val car primitive . car)
            (val cdr primitive . cdr)
            (val procedure? primitive . procedure?))) (val list closure (lambda x x) ())
            (val not primitive . not) (val equal? primitive . equal?)
            (val symbol? primitive . symbol?)
            (val number? primitive . number?)
            (val cons primitive . cons) (val null? primitive . null?)
            (val pair? primitive . pair?) (val car primitive . car)
            (val cdr primitive . cdr)
            (val procedure? primitive . procedure?)))))

(test "evalo-3"
  (run* (val)
    (evalo
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
     val))
  '(#t))

(test "nfo-list?-template-1"
  (run* (expr)
    (nfo
     `(lambda (list?)
        (lambda (l)
          (if (null? l)
              #t
              (if (pair? l)
                  (list? (cdr l))
                  #f))))
     expr))
  '(((lambda (_.0)
       (lambda (_.1)
         (if (null? _.1)
             #t
             (if (pair? _.1)
                 (_.0 (cdr _.1))
                 #f))))
     (=/= ((_.0 _.1))
          ((_.1 closure))
          ((_.1 neutral))
          ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-list?-template-2"
  (run* (expr)
    (nfo
     `(lambda (list?)
        (lambda (l)
          (list? (cdr l))))
     expr))
  '(((lambda (_.0)
       (lambda (_.1)
         (_.0 (cdr _.1))))
     (=/= ((_.0 _.1))
          ((_.1 closure))
          ((_.1 neutral))
          ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-list?-template-3"
  (run* (expr)
    (nfo
     `(lambda (list? l)
        (list? (cdr l)))
     expr))
  '(((lambda (_.0 _.1)
       (_.0 (cdr _.1)))
     (=/= ((_.0 _.1))
          ((_.1 closure))
          ((_.1 neutral))
          ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-template-4"
  (run* (expr)
    (nfo
     `(lambda (f l)
        (f (car l) (cdr l)))
     expr))
  '(((lambda (_.0 _.1)
       (_.0 (car _.1) (cdr _.1)))
     (=/= ((_.0 _.1))
          ((_.1 closure))
          ((_.1 neutral))
          ((_.1 primitive)))
     (sym _.0 _.1))))

(test "nfo-template-5"
  (run* (expr)
    (nfo
     `(lambda (l)
        (null? (car l)))
     expr))
  '(((lambda (_.0)
       (null? (car _.0)))
     (sym _.0))))


(test "evalo-4"
  (run* (val)
    (evalo
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
     val))
  '(#f))

(test "evalo-5"
  (run* (val)
    (evalo
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
     val))
  '(#f))

(test "evalo-6"
  (run 5 (l)
    (evalo
     `((lambda (Z)
         ((lambda (F)
            ((Z F) ',l))
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
     #t))
  '(()
    ((_.0)
     (absento (closure _.0) (neutral _.0) (primitive _.0)))
    ((_.0 _.1)
     (absento (closure _.0) (closure _.1) (neutral _.0)
              (neutral _.1) (primitive _.0) (primitive _.1)))
    ((_.0 _.1 _.2)
     (absento (closure _.0) (closure _.1) (closure _.2) (neutral _.0)
              (neutral _.1) (neutral _.2) (primitive _.0) (primitive _.1)
              (primitive _.2)))
    ((_.0 _.1 _.2 _.3)
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (neutral _.0) (neutral _.1) (neutral _.2) (neutral _.3)
              (primitive _.0) (primitive _.1) (primitive _.2)
              (primitive _.3)))))

(test "evalo-7"
  (run 5 (l)
    (evalo
     `((lambda (Z)
         ((lambda (F)
            ((Z F) ,l))
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
     #t))
  '('()
    ('(_.0)
     (absento (closure _.0) (neutral _.0) (primitive _.0)))
    ('(_.0 _.1)
     (absento (closure _.0) (closure _.1) (neutral _.0)
              (neutral _.1) (primitive _.0) (primitive _.1)))
    ('(_.0 _.1 _.2)
     (absento (closure _.0) (closure _.1) (closure _.2) (neutral _.0)
              (neutral _.1) (neutral _.2) (primitive _.0) (primitive _.1)
              (primitive _.2)))
    ('(_.0 _.1 _.2 _.3)
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (neutral _.0) (neutral _.1) (neutral _.2) (neutral _.3)
              (primitive _.0) (primitive _.1) (primitive _.2)
              (primitive _.3)))))

(test "evalo-8"
  (run* (q)
    (evalo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        (list? '(a b c d e)))
     q))
  '(#t))

(test "evalo-9"
  (run* (q)
    (evalo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        (list? '(a b c d . e)))
     q))
  '(#f))

(test "evalo-10"
  (run 5 (l)
    (evalo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        (list? ,l))
     #t))
  '('()
    ('(_.0)
     (absento (closure _.0) (neutral _.0) (primitive _.0)))
    ('(_.0 _.1)
     (absento (closure _.0) (closure _.1) (neutral _.0)
              (neutral _.1) (primitive _.0) (primitive _.1)))
    ('(_.0 _.1 _.2)
     (absento (closure _.0) (closure _.1) (closure _.2) (neutral _.0)
              (neutral _.1) (neutral _.2) (primitive _.0) (primitive _.1)
              (primitive _.2)))
    ('(_.0 _.1 _.2 _.3)
     (absento (closure _.0) (closure _.1) (closure _.2) (closure _.3)
              (neutral _.0) (neutral _.1) (neutral _.2) (neutral _.3)
              (primitive _.0) (primitive _.1) (primitive _.2)
              (primitive _.3)))))

(test "evalo-11"
  (run* (q)
    (evalo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        list?)
     q))
  '((closure
     (lambda (l)
       (if (null? l)
           #t
           (if (pair? l)
               (list? (cdr l)) #f)))
     ((rec (list? lambda (l)
                  (if (null? l) #t (if (pair? l) (list? (cdr l)) #f))))
      (val list closure (lambda x x) ())
      (val not primitive . not)
      (val equal? primitive . equal?)
      (val symbol? primitive . symbol?)
      (val number? primitive . number?)
      (val cons primitive . cons)
      (val null? primitive . null?)
      (val pair? primitive . pair?)
      (val car primitive . car)
      (val cdr primitive . cdr)
      (val procedure? primitive . procedure?)))))

(test "evalo-append-1"
  (run* (q)
    (evalo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l)
                                   (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((a b c d e)))

(test "nfo-append-2"
  (run* (q)
    (nfo
     `(letrec ((append (lambda (l s)
                         (if (null? l)
                             s
                             (cons (car l)
                                   (append (cdr l) s))))))
        (append '(a b c) '(d e)))
     q))
  '((quote (a b c d e))))

(test "nfo-free-var-1"
  (run* (expr)
    (nfo 'x expr))
  '())

(test "evalo-free-var-1"
  (run* (val)
    (evalo 'x val))
  '())

(test "evalo-free-var-2"
  (run* (val)
    (evalo 'x val))
  '())

(test "unevalo-free-var-1"
  (run* (expr)
    (fresh (x^)
      (unevalo `(,neutral-tag (NVar ,x^)) expr)))
  '((_.0 (sym _.0))))


(test "nfo-list?-1"
  (run* (q)
    (nfo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        (list? '(a b c d e)))
     q))
  '(#t))

(test "nfo-list?-2"
  (run* (q)
    (nfo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        (list? '(a b c d . e)))
     q))
  '(#f))

;; TODO
;;
;; This test appears to diverge, presumably because of the recursion.
;; Verify that this is the correct behavior.
#;(test "nfo-list?-3"
  (run 1 (q)
    (nfo
     `(letrec ((list? (lambda (l)
                        (if (null? l)
                            #t
                            (if (pair? l)
                                (list? (cdr l))
                                #f)))))
        list?)
     q))
  '?)

(test "nfo-simple-letrec-return-function-1"
  (run* (q)
    (nfo
     `(letrec ((f (lambda () 5)))
        f)
     q))
  '((lambda () 5)))

(test "nfo-simple-letrec-return-function-2"
  (run* (q)
    (nfo
     `(letrec ((f (lambda () (cons 5 (cons 'cat '())))))
        f)
     q))
  '((lambda () '(5 cat))))

(test "nfo-simple-letrec-return-function-3"
  (run* (q)
    (nfo
     `(letrec ((f (lambda (x) (cons x (cons 'cat '())))))
        f)
     q))
  '(((lambda (_.0) (cons _.0 '(cat)))
     (=/= ((_.0 closure)) ((_.0 neutral)) ((_.0 primitive)))
     (sym _.0))))

;; TODO
;;
;; This test appears to diverge, presumably because of the recursion.
;; Verify that this is the correct behavior.
#;(test "nfo-simple-letrec-return-function-4"
  (run 1 (q)
    (nfo
     `(letrec ((f (lambda (x) (f x))))
        f)
     q))
  '???)

(test "nfo-simple-letrec-return-function-5"
  (run* (q)
    (nfo
     `(letrec ((f (lambda (x) (if #f (f x) 5))))
        f)
     q))
  '(((lambda (_.0) 5) (sym _.0))))

;; TODO
;;
;; This test appears to diverge, presumably because of the recursion.
;; Verify that this is the correct behavior.
#;(test "nfo-simple-letrec-return-function-6"
  (run 1 (q)
    (nfo
     `(letrec ((f (lambda (x) (if x (f x) 5))))
        f)
     q))
  '(((lambda (_.0) 5) (sym _.0))))


;; Interesting test!
;;
;; There is no normal form, since I wrote this test by changing
;; 'list?' to 'foo', but missed the recursive call to 'list?'  in the
;; function body.
(test "nfo-mistaken-letrec-1"
  (run* (q)
    (nfo
     `(letrec ((foo (lambda (l)
                      (if (null? l)
                          (cons 1 (cons 'cat '()))
                          (if (pair? l)
                              (list? (cdr l))
                              #f)))))
        foo)
     q))
  '())




(test "uneval-many"
  (length (run 350 (val expr) (unevalo val expr)))
  350)

;; Strange error on the 351st result:
;; Exception in type-index: no matching type constraint with irritant ()
;;
;; This error message comes from (type-index v)
;; in
;; miniKanren-version/faster-miniKanren/mk.scm
;;
;; Is this an error in 'absento'?  An error in reification?
;; Or am I doing something illegal with miniKanren?
(test "uneval-too-many"
  (length (run 1000 (val expr) (unevalo val expr)))
  1000)
