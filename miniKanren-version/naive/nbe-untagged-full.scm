(load "../faster-miniKanren/mk-vicare.scm")
(load "../faster-miniKanren/mk.scm")
(load "../faster-miniKanren/test-check.scm")

;; TODO the code in this file is still being massaged...

;; This code was adapted from
;; `Barliman/cocoa/Barliman/mk-and-rel-interp/interp-simple.scm` from
;; `https://github.com/webyrd/Barliman`

;; The definition of 'letrec' is based based on Dan Friedman's code,
;; using the "half-closure" approach from Reynold's definitional
;; interpreters.

;; Observation:
;;
;; `unevalo` and `evalo` should differ in terms of shadowing behavior:
;; because of alpha-renaming, normal form expressions should
;; never have shadowing.  Might be able to play some interesting games
;; based on this behavior.

(define closure-tag 'closure)
(define prim-tag 'primitive)
(define neutral-tag 'neutral)
(define undefined-tag 'undefined)

(define (evalo expr val)
  (eval-expro expr initial-env val))

(define (eval-expro expr env val)
  (conde

    ((== `(quote ,val) expr)
     (absento closure-tag val)
     (absento prim-tag val)
     (absento neutral-tag val)
     (not-in-envo 'quote env))

    ((numbero expr) (== expr val))

    ((symbolo expr) (lookupo expr env val))

    ((fresh (x body)
       (== `(lambda ,x ,body) expr)
       (== `(,closure-tag (lambda ,x ,body) ,env) val)
       (paramso x)
       (not-in-envo 'lambda env)))

    ((fresh (rator rands proc a*)
       (== `(,rator . ,rands) expr)
       (eval-expro rator env proc)
       (eval-listo rands env a*)
       (apply-proco proc a* env val)))

    ;; TODO handle commented cases, below:
    ;; add neutral terms,  and uneval these terms,
    ;; as appropriate
    ;;
    ;; `letrec` is the most important of the
    ;; commented cases
    
    #;((handle-matcho expr env val))

    #;((fresh (begin-body)
       (== `(begin . ,begin-body) expr)
       (not-in-envo 'begin env)
       (eval-begino '() begin-body env val)))

    #;((fresh (b* letrec-body)
       (== `(letrec ,b* ,letrec-body) expr)
       (not-in-envo 'letrec env)
       (eval-letreco b* letrec-body env val)))

    #;((fresh (b* body)
       (== `(let ,b* ,body) expr)
       (not-in-envo 'let env)
       (let loop ((b* b*) (p* '()) (rand* '()))
         (conde
           ((fresh (a* res)
              (== '() b*)
              (eval-listo rand* env a*)
              (ext-env*o p* a* env res)
              (eval-expro body res val)))
           ((fresh (p rand b*-rest)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (loop b*-rest (cons p p*) (cons rand rand*))))))))

    #;((fresh (b* body)
       (== `(let* ,b* ,body) expr)
       (not-in-envo 'let env)
       (let loop ((b* b*) (env env))
         (conde
           ((== '() b*) (eval-expro body env val))
           ((fresh (p rand a b*-rest res)
              (== `((,p ,rand) . ,b*-rest) b*)
              (symbolo p)
              (== `((val . (,p . ,a)) . ,env) res)
              (loop b*-rest res)
              (eval-expro rand env a)))))))

    #;((fresh (qq-expr)
       (== (list 'quasiquote qq-expr) expr)
       (not-in-envo 'quasiquote env)
       (eval-qq-expo 0 qq-expr env val)))

    #;((cond-primo expr env val))

    ((prim-expo expr env val))))

(define apply-proco
  (lambda (proc a* env val)
    (conde
      ((fresh (n)
         ;; Neutral
         (== `(,neutral-tag ,n) proc)
         (== `(,neutral-tag (NApp ,n ,a*)) val)))
      ((fresh (prim-id)
         ;; Primitive
         (== `(,prim-tag . ,prim-id) proc)
         (eval-primo prim-id a* val)))
      ((fresh (x* body env^ res)
         ;; Multi-argument
         (== `(,closure-tag (lambda ,x* ,body) ,env^) proc)
         (ext-env*o x* a* env^ res)
         (eval-expro body res val)))
      ((fresh (x body env^ res)
         ;; Variadic
         (== `(,closure-tag (lambda ,x ,body) ,env^) proc)
         (symbolo x)
         (== `((val . (,x . ,a*)) . ,env^) res)
         (eval-expro body res val))))))

(define empty-env '())

(define (lookup-reco k renv x b* t)
    (conde
      ((== '() b*) (k))
      ((fresh (b*-rest p-name lam-expr)
         (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
         (conde
           ((== p-name x) (== `(,closure-tag ,lam-expr ,renv) t))
           ((=/= p-name x) (lookup-reco k renv x b*-rest t)))))))
(define (lookupo x env t)
  (conde
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (conde
         ((== x y) (== b t))
         ((=/= x y) (lookupo x rest t)))))
    ((fresh (b* rest)
       (== `((rec . ,b*) . ,rest) env)
       (lookup-reco (lambda () (lookupo x rest t)) env x b* t)))))

(define (not-in-envo x env)
  (conde
    ((== empty-env env))
    ((fresh (y b rest)
       (== `((val . (,y . ,b)) . ,rest) env)
       (=/= x y)
       (not-in-envo x rest)))
    ((fresh (b* rest)
       (== `((rec . ,b*) . ,rest) env)
       (not-in-env-reco x b* rest)))))

(define (not-in-env-reco x b* env)
  (conde
    ((== '() b*) (not-in-envo x env))
    ((fresh (p-name lam-expr b*-rest)
       (== `((,p-name . ,lam-expr) . ,b*-rest) b*)
       (=/= p-name x)
       (not-in-env-reco x b*-rest env)))))

#;(define (eval-letreco b* letrec-body env val)
  (let loop ((b* b*) (rb* '()))
    (conde
      ((== '() b*) (eval-expro letrec-body `((rec . ,rb*) . ,env) val))
      ((fresh (p-name x body b*-rest)
         (== `((,p-name (lambda ,x ,body)) . ,b*-rest) b*)
         (symbolo p-name)
         (paramso x)
         (loop b*-rest `((,p-name . (lambda ,x ,body)) . ,rb*)))))))

;; NOTE: rec-defs is Scheme state, not a logic term!
#;(define (eval-begino rec-defs begin-body env val)
  (conde
    ((fresh (e)
        (== `(,e) begin-body)
        (if (null? rec-defs)
          (eval-expro e env val)
          (eval-letreco rec-defs e env val))))
    ((fresh (name args body begin-rest)
        (== `((define ,name (lambda ,args ,body)) . ,begin-rest) begin-body)
        (symbolo name)
        (eval-begino
          (cons `(,name (lambda ,args ,body)) rec-defs) begin-rest env val)))))

;; 'level' is non-relational.
#;(define (eval-qq-expo level qq-expr env val)
  (conde
    ((fresh (expr)
       (== (list 'unquote expr) qq-expr)
       (if (= 0 level)
         (eval-expro expr env val)
         (fresh (sub-val)
           (== (list 'unquote sub-val) val)
           (eval-qq-expo (- level 1) expr env sub-val)))))
    ((fresh (expr sub-val)
       (== (list 'quasiquote expr) qq-expr)
       (== (list 'quasiquote sub-val) val)
       (eval-qq-expo (+ level 1) expr env sub-val)))
    ((fresh (qq-a qq-d va vd)
       (== `(,qq-a . ,qq-d) qq-expr)
       (== `(,va . ,vd) val)
       (=/= 'unquote qq-a)
       (=/= 'quasiquote qq-a)
       (=/= closure-tag qq-a)
       (=/= prim-tag qq-a)
       (=/= neutral-tag qq-a)
       (eval-qq-expo level qq-a env va)
       (eval-qq-expo level qq-d env vd)))
    ((== qq-expr val)
     (conde
       ((== '() val))
       ((symbolo val))
       ((== #f val))
       ((== #t val))
       ((numbero val))))))

(define (eval-listo expr env val)
  (conde
    ((== '() expr)
     (== '() val))
    ((fresh (a d v-a v-d)
       (== `(,a . ,d) expr)
       (== `(,v-a . ,v-d) val)
       (eval-expro a env v-a)
       (eval-listo d env v-d)))))

(define (paramso params)
  (conde
    ; Multiple argument
    ((list-of-paramso params))
    ; Variadic
    ((symbolo params))))

;; TODO can this be replaced with a single `absento` call?
(define (not-in-paramso x params)
  (conde
    ((== '() params))
    ((fresh (a d)
       (== `(,a . ,d) params)
       (=/= a x)
       (not-in-paramso x d)))))

(define (list-of-paramso los)
  (conde
    ((== '() los))
    ((fresh (a d)
       (== `(,a . ,d) los)
       (symbolo a)
       (list-of-paramso d)
       (not-in-paramso a d)))))

(define (ext-env*o x* a* env out)
  (conde
    ((== '() x*) (== '() a*) (== env out))
    ((fresh (x a dx* da* env2)
       (== `(,x . ,dx*) x*)
       (== `(,a . ,da*) a*)
       (== `((val . (,x . ,a)) . ,env) env2)
       (symbolo x)
       (ext-env*o dx* da* env2 out)))))

(define (eval-primo prim-id a* val)
  (conde
    ;; TODO finish implementing the commented clauses
    ;;
    #;[(== prim-id 'cons)
     (fresh (a d)
       (== `(,a ,d) a*)
       (== `(,a . ,d) val)
       (=/= closure-tag a)
       (=/= prim-tag a)
       (=/= neutral-tag a))]
    #;[(== prim-id 'car)
     (fresh (d)
       (== `((,val . ,d)) a*)
       (=/= closure-tag val)
       (=/= prim-tag val)
       (=/= neutral-tag val))]
    #;[(== prim-id 'cdr)
     (fresh (a)
       (== `((,a . ,val)) a*)
       (=/= closure-tag a)
       (=/= prim-tag a)
       (=/= neutral-tag a))]
    #;[(== prim-id 'not)
     (fresh (b)
       (== `(,b) a*)
       (conde
         ((=/= #f b) (== #f val))
         ((== #f b) (== #t val))))]
    #;[(== prim-id 'equal?)
     (fresh (v1 v2)
       (== `(,v1 ,v2) a*)
       (conde
         ((== v1 v2) (== #t val))
         ((=/= v1 v2) (== #f val))))]
    [(== prim-id 'symbol?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (=/= closure-tag a)
            (=/= prim-tag a)
            (=/= neutral-tag a)))
         ((== '() v) (== #f val))
         ((symbolo v) (== #t val))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((numbero v) (== #f val))
         ((fresh (d) (== `(,closure-tag . ,d) v))
          (== #f val))
         ((fresh (d) (== `(,prim-tag . ,d) v))
          (== #f val))
         ((fresh (n)
            (== `(,neutral-tag ,n) v)
            (== `(,neutral-tag (NSymbol? ,n)) val)))))]
    [(== prim-id 'number?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (=/= closure-tag a)
            (=/= prim-tag a)
            (=/= neutral-tag a)))
         ((== '() v) (== #f val))
         ((symbolo v) (== #f val))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((numbero v) (== #t val))
         ((fresh (d) (== `(,closure-tag . ,d) v))
          (== #f val))
         ((fresh (d) (== `(,prim-tag . ,d) v))
          (== #f val))
         ((fresh (n)
            (== `(,neutral-tag ,n) v)
            (== `(,neutral-tag (NNumber? ,n)) val)))))]
    [(== prim-id 'null?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (=/= closure-tag a)
            (=/= prim-tag a)
            (=/= neutral-tag a)))
         ((== '() v) (== #t val))
         ((symbolo v) (== #f val))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((numbero v) (== #f val))
         ((fresh (d) (== `(,closure-tag . ,d) v))
          (== #f val))
         ((fresh (d) (== `(,prim-tag . ,d) v))
          (== #f val))
         ((fresh (n)
            (== `(,neutral-tag ,n) v)
            (== `(,neutral-tag (NNull? ,n)) val)))))]
    [(== prim-id 'pair?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #t val)
            (=/= closure-tag a)
            (=/= prim-tag a)
            (=/= neutral-tag a)))
         ((== '() v) (== #f val))
         ((symbolo v) (== #f val))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((numbero v) (== #f val))
         ((fresh (d) (== `(,closure-tag . ,d) v))
          (== #f val))
         ((fresh (d) (== `(,prim-tag . ,d) v))
          (== #f val))
         ((fresh (n)
            (== `(,neutral-tag ,n) v)
            (== `(,neutral-tag (NPair? ,n)) val)))))]
    [(== prim-id 'procedure?)
     (fresh (v)
       (== `(,v) a*)
       (conde
         ((fresh (a d)
            (== `(,a . ,d) v)
            (== #f val)
            (=/= closure-tag a)
            (=/= prim-tag a)
            (=/= neutral-tag a)))
         ((== '() v) (== #f val))
         ((symbolo v) (== #f val))
         ((== #f v) (== #f val))
         ((== #t v) (== #f val))
         ((numbero v) (== #f val))
         ((fresh (d) (== `(,closure-tag . ,d) v))
          (== #t val))
         ((fresh (d) (== `(,prim-tag . ,d) v))
          (== #t val))
         ((fresh (n)
            (== `(,neutral-tag ,n) v)
            (== `(,neutral-tag (NProcedure? ,n)) val)))))]))

(define (prim-expo expr env val)
  (conde
    ((boolean-primo expr env val))

    ;; TODO handle commented cases, below:
    ;; add neutral terms,  and uneval these terms,
    ;; as appropriate
    ;;
    ;; `if` is the most important of the
    ;; commented cases
    
    #;((and-primo expr env val))
    #;((or-primo expr env val))
    #;((if-primo expr env val))
    ))

(define (boolean-primo expr env val)
  (conde
    ((== #t expr) (== #t val))
    ((== #f expr) (== #f val))))

#;(define (and-primo expr env val)
  (fresh (e*)
    (== `(and . ,e*) expr)
    (not-in-envo 'and env)
    (ando e* env val)))

#;(define (ando e* env val)
  (conde
    ((== '() e*) (== #t val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expro e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((== #f v)
          (== #f val)
          (eval-expro e1 env v))
         ((=/= #f v)
          (eval-expro e1 env v)
          (ando `(,e2 . ,e-rest) env val)))))))

#;(define (or-primo expr env val)
  (fresh (e*)
    (== `(or . ,e*) expr)
    (not-in-envo 'or env)
    (oro e* env val)))

#;(define (oro e* env val)
  (conde
    ((== '() e*) (== #f val))
    ((fresh (e)
       (== `(,e) e*)
       (eval-expro e env val)))
    ((fresh (e1 e2 e-rest v)
       (== `(,e1 ,e2 . ,e-rest) e*)
       (conde
         ((=/= #f v)
          (== v val)
          (eval-expro e1 env v))
         ((== #f v)
          (eval-expro e1 env v)
          (oro `(,e2 . ,e-rest) env val)))))))

#;(define (if-primo expr env val)
  (fresh (e1 e2 e3 t)
    (== `(if ,e1 ,e2 ,e3) expr)
    (not-in-envo 'if env)
    (eval-expro e1 env t)
    (conde
      ((=/= #f t) (eval-expro e2 env val))
      ((== #f t) (eval-expro e3 env val)))))

#;(define (cond-primo expr env val)
  (fresh (c c*)
    (== `(cond ,c . ,c*) expr)
    (not-in-envo 'cond env)
    (cond-clauseso `(,c . ,c*) env val)))

#;(define (cond-clauseso c* env val)
  (conde
    ((== '() c*) (== undefined-tag val))
    ((fresh (conseq)
       (== `((else ,conseq)) c*)
       (not-in-envo 'else env)
       (eval-expro conseq env val)))
    ((fresh (test conseq c*-rest)
       (== `((,test ,conseq) . ,c*-rest) c*)
       (fresh (v)
         (eval-expro test env v)
         (conde
           ((=/= #f v) (eval-expro conseq env val))
           ((== #f v) (cond-clauseso c*-rest env val))))))))


(define initial-env `((val . (list . (,closure-tag (lambda x x) ,empty-env)))
                      (val . (not . (,prim-tag . not)))
                      (val . (equal? . (,prim-tag . equal?)))
                      (val . (symbol? . (,prim-tag . symbol?)))
                      (val . (number? . (,prim-tag . number?)))
                      (val . (cons . (,prim-tag . cons)))
                      (val . (null? . (,prim-tag . null?)))
                      (val . (pair? . (,prim-tag . pair?)))
                      (val . (car . (,prim-tag . car)))
                      (val . (cdr . (,prim-tag . cdr)))
                      (val . (procedure? . (,prim-tag . procedure?)))
                      . ,empty-env))

#;(define handle-matcho
  (lambda  (expr env val)
    (fresh (against-expr mval clause clauses)
      (== `(match ,against-expr ,clause . ,clauses) expr)
      (not-in-envo 'match env)
      (eval-expro against-expr env mval)
      (match-clauses mval `(,clause . ,clauses) env val))))

#;(define (not-symbolo t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((numbero t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

#;(define (not-numbero t)
  (conde
    ((== #f t))
    ((== #t t))
    ((== '() t))
    ((symbolo t))
    ((fresh (a d)
       (== `(,a . ,d) t)))))

#;(define (self-eval-literalo t)
  (conde
    ((numbero t))
    ((booleano t))))

#;(define (literalo t)
  (conde
    ((numbero t))
    ((symbolo t)
     (=/= closure-tag t)
     (=/= prim-tag t)
     (=/= neutral-tag t))
    ((booleano t))
    ((== '() t))))

#;(define (booleano t)
  (conde
    ((== #f t))
    ((== #t t))))

#;(define (regular-env-appendo env1 env2 env-out)
  (conde
    ((== empty-env env1) (== env2 env-out))
    ((fresh (y v rest res)
       (== `((val . (,y . ,v)) . ,rest) env1)
       (== `((val . (,y . ,v)) . ,res) env-out)
       (regular-env-appendo rest env2 res)))))

#;(define (match-clauses mval clauses env val)
  (fresh (p result-expr d penv)
    (== `((,p ,result-expr) . ,d) clauses)
    (conde
      ((fresh (env^)
         (p-match p mval '() penv)
         (regular-env-appendo penv env env^)
         (eval-expro result-expr env^ val)))
      ((p-no-match p mval '() penv)
       (match-clauses mval d env val)))))

#;(define (var-p-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= closure-tag mval)
    (=/= prim-tag mval)
    (=/= neutral-tag mval)
    (conde
      ((== mval val)
       (== penv penv-out)
       (lookupo var penv val))
      ((== `((val . (,var . ,mval)) . ,penv) penv-out)
       (not-in-envo var penv)))))

#;(define (var-p-no-match var mval penv penv-out)
  (fresh (val)
    (symbolo var)
    (=/= mval val)
    (== penv penv-out)
    (lookupo var penv val)))

#;(define (p-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (== p mval)
     (== penv penv-out))
    ((var-p-match p mval penv penv-out))
    ((fresh (datum)
       (== `(quote ,datum) p)
       (== datum mval)
       (== penv penv-out)))
    ((fresh (var pred val)
      (== `(? ,pred ,var) p)
      (conde
        ((== 'symbol? pred)
         (symbolo mval))
        ((== 'number? pred)
         (numbero mval)))
      (var-p-match var mval penv penv-out)))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-match quasi-p mval penv penv-out)))))

#;(define (p-no-match p mval penv penv-out)
  (conde
    ((self-eval-literalo p)
     (=/= p mval)
     (== penv penv-out))
    ((var-p-no-match p mval penv penv-out))
    ((fresh (datum)
       (== `(quote ,datum) p)
       (=/= datum mval)
       (== penv penv-out)))
    ((fresh (var pred val)
       (== `(? ,pred ,var) p)
       (== penv penv-out)
       (symbolo var)
       (conde
         ((== 'symbol? pred)
          (conde
            ((not-symbolo mval))
            ((symbolo mval)
             (var-p-no-match var mval penv penv-out))))
         ((== 'number? pred)
          (conde
            ((not-numbero mval))
            ((numbero mval)
             (var-p-no-match var mval penv penv-out)))))))
    ((fresh (quasi-p)
      (== (list 'quasiquote quasi-p) p)
      (quasi-p-no-match quasi-p mval penv penv-out)))))

#;(define (quasi-p-match quasi-p mval penv penv-out)
  (conde
    ((== quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
      (== (list 'unquote p) quasi-p)
      (p-match p mval penv penv-out)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (== `(,v1 . ,v2) mval)
       (=/= 'unquote a)
       (quasi-p-match a v1 penv penv^)
       (quasi-p-match d v2 penv^ penv-out)))))

#;(define (quasi-p-no-match quasi-p mval penv penv-out)
  (conde
    ((=/= quasi-p mval)
     (== penv penv-out)
     (literalo quasi-p))
    ((fresh (p)
       (== (list 'unquote p) quasi-p)
       (=/= closure-tag mval)
       (=/= prim-tag mval)
       (=/= neutral-tag mval)
       (p-no-match p mval penv penv-out)))
    ((fresh (a d)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== penv penv-out)
       (literalo mval)))
    ((fresh (a d v1 v2 penv^)
       (== `(,a . ,d) quasi-p)
       (=/= 'unquote a)
       (== `(,v1 . ,v2) mval)
       (conde
         ((quasi-p-no-match a v1 penv penv^))
         ((quasi-p-match a v1 penv penv^)
          (quasi-p-no-match d v2 penv^ penv-out)))))))


;; Fast and simple fresho definition (written with Michael Ballantyne)
;; Rather than compute a renamed variable, we just describe the
;; constraints.
(define fresho
  (lambda (xs x^)
    (fresh ()
      (symbolo x^)
      (absento x^ xs))))

(define quoted-or-self-quotingo
  (lambda (expr datum)
    (conde
      ((== #f expr) (== expr datum))
      ((== #t expr) (== expr datum))
      ((numbero expr) (== expr datum))
      ((== `(quote ,datum) expr)))))

(define not-quoted-and-not-self-quotingo
  (lambda (expr)
    (conde
      ((symbolo expr))
      ((== '() expr))
      ((fresh (a d)
         (== `(,a . ,d) expr)
         (=/= 'quote a))))))

(define (unevalo val expr)
  (uneval-valueo xs val expr))

(define (uneval-valueo xs val expr)
  (conde
    
    ((numbero val) (== v expr))
    
    ((== #f val) (== #f expr))
    ((== #t val) (== #t expr))
    
    ((== '() val) (== '(quote ()) expr))
    
    ((symbolo val)
     (== `(quote ,val) expr)
     (=/= closure-tag val)
     (=/= prim-tag val)
     (=/= neutral-tag val))

    ((fresh (prim-id)
       ;; Primitive operator
       (== `(,prim-tag . ,prim-id) val)
       (== prim-id expr)
       ;; prevent shadowing of the prim-op by variables introduced by
       ;; `fresho`
       (absento prim-id xs)))
    
    ((fresh (n)
       (== `(,neutral-tag ,n) val)
       (uneval-neutralo xs n expr)))

    ((fresh (x body env x^ body^ bv)
       ;; Variadic
       (== `(,closure-tag (lambda ,x ,body) ,env) val)
       (== `(lambda ,x^ ,body^) expr)
       (symbolo x)
       (symbolo x^)
       (fresho xs x^)
       (eval-expro body
                   `((val . (,x . (,neutral-tag (NVar ,x^)))) . ,env)
                   bv)
       (uneval-valueo `(,x^ . ,xs) bv body^)))

    ((fresh (x* body env x*^ body^ env^ bv xs^)
       ;; Multi-argument
       (== `(,closure-tag (lambda ,x* ,body) ,env) val)
       (== `(lambda ,x*^ ,body^) expr)
       (ext-env-neutral*o x* x*^ env env^ xs xs^)
       (eval-expro body env^ bv)
       (uneval-valueo xs^ bv body^)))
    
    ((fresh (v1 v2 e1 e2)
       (== `(,v1 . ,v2) val)
       (=/= closure-tag v1)
       (=/= prim-tag v1)
       (=/= neutral-tag v1)
       (absento closure-tag expr)
       (absento prim-tag expr)
       (absento neutral-tag expr)
       (conde
         ((fresh (d1 d2)
            (== `(quote (,d1 . ,d2)) expr)
            (quoted-or-self-quotingo e1 d1)
            (quoted-or-self-quotingo e2 d2)))
         ((== `(cons ,e1 ,e2) expr)
          (conde
            ((not-quoted-and-not-self-quotingo e1))
            ((fresh (d1)
               (quoted-or-self-quotingo e1 d1)
               (not-quoted-and-not-self-quotingo e2))))))
       (uneval-valueo xs v1 e1)
       (uneval-valueo xs v2 e2)))))

;; TODO
;;
;; adjust the stuck terms to include those from `evalo`
;; including `let`, `let*`, `match`, and `quasiquote`.
;;
;; also, see if I can represent all primitive applications
;; as a single neutral term that includes the prim op name
(define uneval-neutralo
  (lambda (xs n expr)
    ;; TODO implement me!
    'TODO
    ))

(define ext-env-neutral*o
  (lambda (x* x*^ env env^ xs xs^)
    (conde
      ((== '() x*) (== '() x*^) (== env env^) (== xs xs^))
      ((fresh (x x^ x*-rest x*^-rest)
         (== `(,x . ,x*-rest) x*)
         (== `(,x^ . ,x*^-rest) x*^)
         (symbolo x)
         (symbolo x^)
         (fresho xs x^)
         (ext-env-neutral*o
           x*-rest
           x*^-rest
           `((val . (,x . (,neutral-tag (NVar ,x^)))) . ,env)
           env^
           `(,x^ . ,xs)
           xs^))))))

(define nfo
  (lambda (expr expr-normal)
    (fresh (val)
      (evalo expr val)
      (unevalo val expr-normal))))