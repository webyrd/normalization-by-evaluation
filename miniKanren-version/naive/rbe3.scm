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

(test "synthesize iota"
  (time
    (run 1 (iota)
      (rfo `(lambda (f) (,iota f)) `(lambda (f) (f (,iota f))))))
  '((((lambda (_.0) (lambda (_.1) (_.1 ((_.0 _.0) _.1))))
      (lambda (_.2) (lambda (_.3) (_.3 ((_.2 _.2) _.3)))))
     (=/= ((_.0 _.1)) ((_.0 f)) ((_.1 f)) ((_.2 _.3)) ((_.2 f))
          ((_.3 f)))
     (sym _.0 _.1 _.2 _.3))))

;; TODO
;; * try synthesizing Z combinator
;; * try to do general beta reduction instead of just beta value
;; (MB hypothesizes this will require introducing neutral terms to the evaluator)
;; * add eta
;; * merge conde clauses, reorder, etc., if helpful
;; * write a pearl!

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
