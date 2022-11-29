(load "nbe-untagged-full.scm")
(load "../faster-miniKanren/test-check.scm")

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
