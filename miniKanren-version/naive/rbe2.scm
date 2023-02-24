(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/test-check.scm")

;; reduction by evaluation, in order to synthesize
;; fixpoint combinators

;; Michael Ballantyne and Will Byrd, 22 Feb 02023

;; This version: inlined and distributed apply-expro to better connect to output

(define lookupo
  (lambda (x env val)
    (fresh (y v env^)
      (== `((,y . ,v) . ,env^) env)
      (symbolo x)
      (symbolo y)
      (conde
        ((== x y) (== v val))
        ((=/= x y)
         (lookupo x env^ val))))))

(define eval-expro
  (lambda (expr env val)
    (conde
      ((fresh (x body)
         (== `(lambda (,x) ,body) expr)
         (== `(closure (,x) ,body ,env) val)
         (symbolo x)))
      ((symbolo expr) (lookupo expr env val))

      ((fresh (e1 e2 f v)
         (== `(,e1 ,e2) expr)
         (== `(NApp ,f ,v) val)
         (eval-expro e1 env f)
         (eval-expro e2 env v)))
      ((fresh (e1 e2 f v x body env^)
         (== `(,e1 ,e2) expr)
         (== `(closure (,x) ,body ,env^) f)
         (symbolo x)
         (eval-expro e1 env f)
         (eval-expro e2 env v)
         (eval-expro body `((,x . ,v) . ,env^) val))))))


;; Fast and simple fresho definition (written with Michael Ballantyne)
;; Rather than compute a renamed variable, we just describe the constraints.
(define fresho
  (lambda (xs x^)
    (fresh ()
      (symbolo x^)
      (absento x^ xs))))

(define uneval-valueo
  (lambda (xs v expr)
    (conde
      ((== `(NVar ,expr) v)
       (symbolo expr))
      ((fresh (n^ v^ ne ve)
         (== `(NApp ,n^ ,v^) v)
         (== `(,ne ,ve) expr)
         (uneval-valueo xs n^ ne)
         (uneval-valueo xs v^ ve)))
      ((fresh (x body env x^ body^ bv)
         (== `(closure (,x) ,body ,env) v)
         (== `(lambda (,x^) ,body^) expr)
         (symbolo x)
         (symbolo x^)
         (fresho xs x^)
         (eval-expro body
                     `((,x . (NVar ,x^)) . ,env)
                     bv)
         (uneval-valueo `(,x^ . ,xs) bv body^))))))

(define rfo
  (lambda (t expr)
    (fresh (v)
      (eval-expro t '() v)
      (uneval-valueo '() v expr))))

(run 5 (q)
  (rfo q '(lambda (x) x)))
;; =>
'(((lambda (_.0) _.0)
   (sym _.0))
  (((lambda (_.0) (lambda (_.1) _.1)) (lambda (_.2) _.3))
   (sym _.0 _.1 _.2))
  ((lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.3)))
   (=/= ((_.0 _.1)))
   (sym _.0 _.1 _.2))
  (((lambda (_.0) _.0) (lambda (_.1) _.1)) (sym _.0 _.1))
  ((lambda (_.0) ((lambda (_.1) _.1) _.0)) (sym _.0 _.1)))

(run* (q)
  (rfo '(lambda (x) ((lambda (y) x) (lambda (z) w))) q))
;; =>
'(((lambda (_.0) _.0) (sym _.0)))

(run 1 (q)
  (fresh (t)
    (eval-expro `(lambda (x) ((lambda (y) x) (lambda (z) ,t))) '() q)))
;; =>
'((closure (x) ((lambda (y) x) (lambda (z) w)) ()))

(run 2 (q)
  (eval-expro '((lambda (y) x) (lambda (z) w)) '((x . (NVar x^))) q))
;; =>
'((NApp
   (closure (y) x ((x NVar x^)))
   (closure (z) w ((x NVar x^))))
  (NVar x^))

(run 3 (t1 t2)
  (fresh (t)
    (== `(lambda (x) ((lambda (y) x) (lambda (z) ,t))) t1)
    (rfo t1 t2)))
;; =>
'((((lambda (x) ((lambda (y) x) (lambda (z) _.0)))
    (lambda (_.1) _.1))
   (sym _.1))
  (((lambda (x) ((lambda (y) x) (lambda (z) z)))
    (lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.2))))
   (=/= ((_.0 _.1)) ((_.0 _.2)))
   (sym _.0 _.1 _.2))
  (((lambda (x) ((lambda (y) x) (lambda (z) x)))
    (lambda (_.0) ((lambda (_.1) _.0) (lambda (_.2) _.0))))
   (=/= ((_.0 _.1)) ((_.0 _.2)))
   (sym _.0 _.1 _.2)))

(run 3 (t1 t2)
  (fresh (t)
    (== t1 t2)
    (== `(lambda (x) ((lambda (y) x) (lambda (z) ,t))) t1)
    (rfo t1 t2)))
;; =>
'(((lambda (x) ((lambda (y) x) (lambda (z) z)))
   (lambda (x) ((lambda (y) x) (lambda (z) z))))
  ((lambda (x) ((lambda (y) x) (lambda (z) x)))
   (lambda (x) ((lambda (y) x) (lambda (z) x))))
  (((lambda (x)
      ((lambda (y) x) (lambda (z) (lambda (_.0) _.0))))
    (lambda (x)
      ((lambda (y) x) (lambda (z) (lambda (_.0) _.0)))))
   (=/= ((_.0 x)) ((_.0 z)))
   (sym _.0)))

#|
(run 1 (Y t)
  (rfo `(lambda (f) (,Y f)) t)
  (rfo `(lambda (f) (f (,Y f))) t))
|#

#|
;; Call-by-name Y combinator
(lambda (f)
  ((lambda (x) (f (x x)))
   (lambda (x) (f (x x)))))
|#

(run 1 (Y t)
  (== '(lambda (f)
         ((lambda (x) (f (x x)))
          (lambda (x) (f (x x)))))
      Y)
  (rfo `(lambda (f) (,Y f)) t)
  (rfo `(lambda (f) (f (,Y f))) t))
;; =>
'((((lambda (f)
      ((lambda (x) (f (x x)))
       (lambda (x) (f (x x)))))
    (lambda (_.0)
      (_.0 ((lambda (_.1) (_.0 (_.1 _.1)))
            (lambda (_.2) (_.0 (_.2 _.2)))))))
   (=/= ((_.0 _.1)) ((_.0 _.2)))
   (sym _.0 _.1 _.2)))

(run 1 (Y t)
  (fresh (?)
    (== `(lambda (f)
           ((lambda (x) (f (x x)))
            (lambda (x) (f (,? ,?)))))
        Y))
  (rfo `(lambda (f) (,Y f)) t)
  (rfo `(lambda (f) (f (,Y f))) t))
;; =>
'((((lambda (f)
     ((lambda (x) (f (x x))) (lambda (x) (f (x x)))))
    (lambda (_.0)
      (_.0 ((lambda (_.1) (_.0 (_.1 _.1)))
             (lambda (_.2) (_.0 (_.2 _.2)))))))
   (=/= ((_.0 _.1)) ((_.0 _.2)))
   (sym _.0 _.1 _.2)))

#|
(run 1 (Y t)
  (fresh (?)
    (== `(lambda (f)
           ((lambda (x) (f ,?))
            (lambda (x) (f (x x)))))
        Y))
  (rfo `(lambda (f) (,Y f)) t)
  (rfo `(lambda (f) (f (,Y f))) t))
|#

;; Omega reduces to itself
(run 2 (t1 t2)
  (== '((lambda (x) (x x))
        (lambda (x) (x x)))
      t1)
  (rfo t1 t2))
;; =>
'(((((lambda (x) (x x)) (lambda (x) (x x)))
    ((lambda (_.0) (_.0 _.0)) (lambda (_.1) (_.1 _.1))))
   (sym _.0 _.1))
  ((((lambda (x) (x x)) (lambda (x) (x x)))
    ((lambda (_.0) (_.0 _.0)) (lambda (_.1) (_.1 _.1))))
   (sym _.0 _.1)))

;; Challenge:  how to generate Omega?
;; MB suggests that using De Bruijn
;; would work.
(run 1 (t1 t2)
  (=/= t1 t2)
  (rfo t1 t2)
  (rfo t2 t1))
;; =>
'((((lambda (_.0) _.0)
    (lambda (_.1) _.1))
   (=/= ((_.0 _.1)))
   (sym _.0 _.1)))

(run 1 (t1 t2)
  (fresh (e1 e2)
    (== `(,e1 ,e2) t1))
  (=/= t1 t2)
  (rfo t1 t2)
  (rfo t2 t1))
;; =>
'(((((lambda (_.0) _.0) (lambda (_.1) _.1))
    ((lambda (_.2) _.2) (lambda (_.3) _.3)))
   (=/= ((_.0 _.2) (_.1 _.3)))
   (sym _.0 _.1 _.2 _.3)))


;; iota from wiki: https://en.wikipedia.org/wiki/Fixed-point_combinator
(test "check iota"
  (run 1 (iota)
    (== '((lambda (x) (lambda (y) (y ((x x) y))))
          (lambda (x) (lambda (y) (y ((x x) y)))))
        iota)
    (rfo `(lambda (f) (,iota f)) `(lambda (f) (f (,iota f)))))
  '(((lambda (x) (lambda (y) (y ((x x) y))))
     (lambda (x) (lambda (y) (y ((x x) y)))))))

(test "synthesize a little bit of iota"
  (time
   (run 1 (iota)
     (fresh (?)
       (== `((lambda (x) (lambda (y) (y (,? y))))
             (lambda (x) (lambda (y) (y ((x x) y)))))
           iota))
     (rfo `(lambda (f) (,iota f)) `(lambda (f) (f (,iota f))))))
  '(((lambda (x) (lambda (y) (y ((x x) y))))
     (lambda (x) (lambda (y) (y ((x x) y)))))))

