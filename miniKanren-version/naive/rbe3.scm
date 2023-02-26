(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/test-check.scm")

;; reduction by evaluation, in order to synthesize
;; fixpoint combinators

;; Michael Ballantyne and Will Byrd, 22 Feb 02023

;; This version: combine eval and uneval into reduceo, which is used whenever
;; the eval is being done for immediate uneval. This better connects to the output reduced expression.

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


(define (reduceo xs expr env expr^)
  (conde
    ;; lambda stays a lambda
    [(fresh (x body x^ body^)
       (== `(lambda (,x) ,body) expr)
       (== `(lambda (,x^) ,body^) expr^)
       (symbolo x)
       (symbolo x^)
       (fresho xs x^)
       (reduceo `(,x^ . ,xs)
                body
                `((,x . (NVar ,x^)) . ,env)
                body^))]

    ;; var stays a var
    [(symbolo expr)
     (symbolo expr^)
     (lookupo expr env `(NVar ,expr^))]
    
    ;; var looks up to a closure
    [(fresh (x body x^ body^ env^)
       (symbolo expr)
       (== `(lambda (,x^) ,body^) expr^)
       (symbolo x^)
       (fresho xs x^)
       (lookupo expr env `(closure (,x) ,body ,env^))
       (reduceo `(,x^ . ,xs)
                body
                `((,x . (NVar ,x^)) . ,env)
                body^))]

    ;; app stays an app
    [(fresh (e1 e2 e1^ e2^)
       (== `(,e1 ,e2) expr)
       (== `(,e1^ ,e2^) expr^)
       (reduceo xs e1 env e1^)
       (reduceo xs e2 env e2^))]

    ;; app reduces

    ;; It makes sense that to reduce an appplication, we have to
    ;; obtain a value for the rator.  But, we also force the operand
    ;; to reduce. I think this means that this version is implementing
    ;; Beta-value reduction, not general beta.
    [(fresh (e1 e2 f v x body env^)
       (== `(,e1 ,e2) expr)
       (== `(closure (,x) ,body ,env^) f)
       (symbolo x)
       (eval-expro e1 env f)
       (eval-expro e2 env v)
       (reduceo xs body `((,x . ,v) . ,env^) expr^))]))

(define rfo
  (lambda (e e^)
    (reduceo '() e '() e^)))

(test "check it's Beta-v"
  (run* (q) (rfo '(lambda (f) ((lambda (x) x) ((lambda (x) (x x)) f))) q))
  '(
    ;; reduced in operand; note that (_.0 _.0) cannot be further reduced because the value of the variable is unknown
    ((lambda (_.0) ((lambda (_.1) _.1) (_.0 _.0)))
     (=/= ((_.0 _.1)))
     (sym _.0 _.1))

    ;; not reduced at all
    ((lambda (_.0)
       ((lambda (_.1) _.1) ((lambda (_.2) (_.2 _.2)) _.0)))
     (=/= ((_.0 _.1)) ((_.0 _.2)))
     (sym _.0 _.1 _.2))

    ;; no additional answer where the top-level application is reduced
    ))

;; theta from wiki: https://en.wikipedia.org/wiki/Fixed-point_combinator
(test "check theta"
  (run 1 (theta)
    (== '((lambda (x) (lambda (y) (y ((x x) y))))
          (lambda (x) (lambda (y) (y ((x x) y)))))
        theta)
    (rfo `(lambda (f) (,theta f)) `(lambda (f) (f (,theta f)))))
  '(((lambda (x) (lambda (y) (y ((x x) y))))
     (lambda (x) (lambda (y) (y ((x x) y)))))))

(test "synthesize a little bit of theta"
  (time
    (run 1 (theta)
      (fresh (?)
             (== `((lambda (x) (lambda (y) (y (,? y))))
                   (lambda (x) (lambda (y) (y ((x x) y)))))
                 theta))
      (rfo `(lambda (f) (,theta f)) `(lambda (f) (f (,theta f))))))
  '(((lambda (x) (lambda (y) (y ((x x) y))))
     (lambda (x) (lambda (y) (y ((x x) y)))))))

(test "synthesize theta"
  (time
    (run 1 (theta)
      (rfo `(lambda (f) (,theta f)) `(lambda (f) (f (,theta f))))))
  '((((lambda (_.0) (lambda (_.1) (_.1 ((_.0 _.0) _.1))))
      (lambda (_.2) (lambda (_.3) (_.3 ((_.2 _.2) _.3)))))
     (=/= ((_.0 _.1)) ((_.0 f)) ((_.1 f)) ((_.2 _.3)) ((_.2 f))
          ((_.3 f)))
     (sym _.0 _.1 _.2 _.3))))

(test "synthesize Y from (lambda (F) (,? ,?)), where ? is the U combinator"
  (time (run 1 (Y t)
          (fresh (?)
            (== `(lambda (f) (,? ,?)) Y))
          (rfo `(lambda (f) (,Y f)) t)
          (rfo `(lambda (f) (f (,Y f))) t)))
  '((((lambda (f)
        ((lambda (_.0) (f (_.0 _.0))) (lambda (_.0) (f (_.0 _.0)))))
      (lambda (_.1)
        (_.1 (_.1 ((lambda (_.2) (_.1 (_.2 _.2)))
                   (lambda (_.3) (_.1 (_.3 _.3))))))))
     (=/= ((_.0 f)) ((_.1 _.2)) ((_.1 _.3)))
     (sym _.0 _.1 _.2 _.3))))
;; (time (run 1 ...))
;;     1680 collections
;;     152.811652658s elapsed cpu time, including 12.961612693s collecting
;;     152.823347000s elapsed real time, including 12.968673000s collecting
;;     14080950784 bytes allocated, including 13852423440 bytes reclaimed

;; This version seems to take a long time...
#|
(time (run 1 (Y t)
        (fresh (?1 ?2)
          (== `(lambda (f) (,?1 ,?2)) Y))
        (rfo `(lambda (f) (,Y f)) t)
        (rfo `(lambda (f) (f (,Y f))) t)))
|#

;; Note that the (f (,Y f)) term coming before (,Y f)
;; speeds up at least some of these tests
(test "synthesize Y, 2"
  (time
   (run 1 (Y t)
     (fresh (?1 ?2)
       (== `(lambda (f)
              (,?1
               (lambda (x) (f (x ,?2))))) Y))
     (rfo `(lambda (f) (f (,Y f))) t)
     (rfo `(lambda (f) (,Y f)) t)))
  '((((lambda (f)
        ((lambda (_.0) (_.0 _.0)) (lambda (x) (f (x x)))))
      (lambda (_.1)
        (_.1 (_.1 ((lambda (_.2) (_.1 (_.2 _.2)))
                   (lambda (_.3) (_.1 (_.3 _.3))))))))
     (=/= ((_.1 _.2)) ((_.1 _.3)))
     (sym _.0 _.1 _.2 _.3))))
;; (time (run 1 ...))
;;     512 collections
;;     8.432182553s elapsed cpu time, including 2.167394615s collecting
;;     8.432332000s elapsed real time, including 2.168518000s collecting
;;     4292458960 bytes allocated, including 4751902880 bytes reclaimed

;; seems to take a long time
#|
(time
 (run 1 (Y t)
   (fresh (?1 ?2)
     (== `(lambda (f)
            (,?1
             (lambda (x) (f ,?2)))) Y))
   (rfo `(lambda (f) (f (,Y f))) t)
   (rfo `(lambda (f) (,Y f)) t)))
|#

(test "Y-4"
  (time
   (run 1 (Y t)
     (fresh (body ?1 ?2)
       (== `(lambda (f)
              ((lambda (x) ,?1)
               (lambda (x) (f (x ,?2))))) Y)
       (== `(lambda (f) ,body) t)
       (rfo `((lambda (f) (f (,Y f))) (lambda (f) (,Y f)))
            `(,t ,t))
       (rfo `(lambda (f) (f (,Y f))) t)
       (rfo `(lambda (f) (,Y f)) t))))
  '((((lambda (f) ((lambda (x) (x x)) (lambda (x) (f (x x)))))
      (lambda (f)
        (f (f ((lambda (_.0) (f (_.0 _.0)))
               (lambda (_.1) (f (_.1 _.1))))))))
     (=/= ((_.0 f)) ((_.1 f)))
     (sym _.0 _.1))))
;; (time (run 1 ...))
;;     929 collections
;;     13.202181094s elapsed cpu time, including 2.718599906s collecting
;;     13.203039000s elapsed real time, including 2.720404000s collecting
;;     7812939584 bytes allocated, including 7615335136 bytes reclaimed



;; TODO
;; * try synthesizing Z combinator
;; * try to do general beta reduction instead of just beta value
;; (MB hypothesizes this will require introducing neutral terms to the evaluator)
;; * add eta
;; * create De Bruijn version, which should make it easier to represent
;;   Omega synthesis, for example
;; * merge conde clauses, reorder, etc., if helpful
;; * add depth limited search
;; * write a pearl!
