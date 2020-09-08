(load "nbe.scm")
(load "../../scheme-helpers/test-macro.scm")

(test "ntho-1"
  (run* (q) (ntho 'z '() q))
  '())

(test "ntho-2"
  (run* (q) (ntho 'z '(42) q))
  '(42))

(test "ntho-3"
  (run* (q) (ntho 'z '(42 137) q))
  '(42))

(test "ntho-4"
  (run* (q) (ntho '(s z) '(42 137) q))
  '(137))

(test "ntho-5"
  (run* (q) (ntho '(s z) '(42) q))
  '())

(test "ntho-6"
  (run* (q) (ntho '(s (s z)) '(42 137 31) q))
  '(31))

(test "ntho-7"
  (run* (q) (ntho q '(42 137 31) 31))
  '((s (s z))))

(test "ntho-8"
  (run* (q) (ntho '(s (s z)) q 31))
  '((_.0 _.1 31 . _.2)))

(test "minuso-1"
  (run* (q)
    (minuso '(s (s (s (s (s z))))) '(s (s (s z))) q))
  '((s (s z))))

(test "minuso-2"
  (run* (q)
    (minuso '(s (s (s z))) q '(s (s (s (s (s z)))))))
  '())

(test "minuso-3"
  (run 3 (n)
    (minuso n n 'z))
  '(z (s z) (s (s z))))

#|
;;; WEB - diverges instead of fails, alas
(test "minuso-4"
  (run 1 (n n2)
    (minuso n n '(s ,n2)))
  '???)
|#

(test "evalo-1"
  (run* (q) (evalo '() '(Lam (Var z)) q))
  '((Clo () (Var z))))

(test "evalo-2"
  (run* (q) (evalo '() '(Lam (App (Lam (Var z)) (Lam (Var z)))) q))
  '((Clo () (App (Lam (Var z)) (Lam (Var z))))))

(test "evalo-3"
  (run* (q) (evalo '((Lam (Var z))) '(Var z) q))
  '((Lam (Var z))))

(test "evalo-4"
  (run* (q) (evalo '((Lam (Var z)) (Clo () (App (Lam (Var z)) (Lam (Var z))))) '(Var (s z)) q))
  '((Clo () (App (Lam (Var z)) (Lam (Var z))))))

(test "evalo-5"
  (run* (q) (evalo '() '(App (Lam (Var z)) (Lam (Var z))) q))
  '((Clo () (Var z))))

(test "unevalo-1"
  (run* (q) (unevalo 'z '(Clo () (Var z)) q))
  '((Lam (Var z))))

(test "unevalo-2"
  (run 1 (v1 v2 n1 n2 e)
    (=/= v1 v2)
    (fresh (e^)
      (== `(App . ,e^) e))
    (unevalo n1 v1 e)
    (unevalo n2 v2 e))
  '(((N (Napp (NVar z) (NVar z)))
     (N (Napp (NVar (s z)) (NVar (s z))))
     (s _.0)
     (s (s _.0))
     (App (Var _.0) (Var _.0)))))

#|
;;; WEB -- I claim that this should diverge!
(test "unevalo-3"
  (run 1 (v1 v2 e)
    (=/= v1 v2)
    (fresh (e^)
      (== `(App . ,e^) e))
    (unevalo 'z v1 e)
    (unevalo 'z v2 e))
  '???)
|#

(test "nfo-1"
  (run 10 (q) (nfo '() q '(Lam (Var z))))
  '((Lam (Var z))
    (App (Lam (Lam (Var z))) (Lam _.0))
    (Lam (App (Lam (Var (s z))) (Lam _.0))) ;; (lambda (x) ((lambda (y) x) (lambda . _.0)))
    (App (Lam (Var z)) (Lam (Var z)))
    (Lam (App (Lam (Var z)) (Var z)))
    (Lam (App (Lam (Var (s z))) (Var z)))
    (App (Lam (Lam (App (Lam (Var (s z))) (Lam _.0)))) (Lam _.1))
    (App (Lam (Lam (App (Lam (Var z)) (Var z)))) (Lam _.0))
    (App (Lam (Var z)) (Lam (App (Lam (Var (s z))) (Lam _.0))))
    (App (Lam (Lam (App (Lam (Var (s z))) (Var z)))) (Lam _.0))))

(test "nfo-2"
  (run 10 (e1 e2 ne)
    (=/= e1 e2)
    (=/= e1 ne)
    (=/= e2 ne)
    (nfo '() e1 ne)
    (nfo '() e2 ne))
  '((((App (Lam (Lam (Var z))) (Lam _.0))
      (App (Lam (Lam (Var z))) (Lam _.1))
      (Lam (Var z)))
     (=/= ((_.0 _.1))))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (Lam (App (Lam (Var (s z))) (Lam _.1)))
     (Lam (Var z)))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (App (Lam (Var z)) (Lam (Var z)))
     (Lam (Var z)))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (Lam (App (Lam (Var z)) (Var z)))
     (Lam (Var z)))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (Lam (App (Lam (Var (s z))) (Var z)))
     (Lam (Var z)))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (App (Lam (Lam (App (Lam (Var (s z))) (Lam _.1))))
          (Lam _.2))
     (Lam (Var z)))
    (((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
      (App (Lam (Lam (Lam (Var z)))) (Lam _.1))
      (Lam (Lam (Var z))))
     (=/= ((_.0 _.1))))
    ((App (Lam (Lam (Var z))) (Lam _.0))
     (App (Lam (Lam (App (Lam (Var z)) (Var z)))) (Lam _.1))
     (Lam (Var z)))
    ((App (Lam (Var z)) (Lam (Var z)))
     (App (Lam (Lam (Var z))) (Lam _.0))
     (Lam (Var z)))
    ((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
     (Lam (App (Lam (Lam (Var z))) (Lam _.1)))
     (Lam (Lam (Var z))))))

(test "nfo-3"
  (run 5 (e1 e2 ne)
    (=/= e1 e2)
    (=/= e1 ne)
    (=/= e2 ne)
    (=/= `(Lam (Var z)) ne)
    (nfo '() e1 ne)
    (nfo '() e2 ne))
  '((((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
      (App (Lam (Lam (Lam (Var z)))) (Lam _.1))
      (Lam (Lam (Var z))))
     (=/= ((_.0 _.1))))
    ((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
     (Lam (App (Lam (Lam (Var z))) (Lam _.1)))
     (Lam (Lam (Var z))))
    ((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
     (Lam (Lam (App (Lam (Var (s z))) (Lam _.1))))
     (Lam (Lam (Var z))))
    ((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
     (App (Lam (Var z)) (Lam (Lam (Var z))))
     (Lam (Lam (Var z))))
    ((App (Lam (Lam (Lam (Var z)))) (Lam _.0))
     (App (Lam (Lam (Var (s z)))) (Lam (Var z)))
     (Lam (Lam (Var z))))))

(test "nfo-4"
  (run 5 (e1 e2 ne)
    (=/= e1 e2)
    (=/= e1 ne)
    (=/= e2 ne)
    (=/= `(Lam (Var z)) ne)
    (=/= `(Lam (Lam (Var z))) ne)
    (nfo '() e1 ne)
    (nfo '() e2 ne))
  '((((App (Lam (Lam (Lam (Var (s z))))) (Lam _.0))
      (App (Lam (Lam (Lam (Var (s z))))) (Lam _.1))
      (Lam (Lam (Var (s z)))))
     (=/= ((_.0 _.1))))
    ((App (Lam (Lam (Lam (Var (s z))))) (Lam _.0))
     (Lam (App (Lam (Lam (Var (s (s z))))) (Lam _.1)))
     (Lam (Lam (Var (s z)))))
    ((App (Lam (Lam (Lam (Var (s z))))) (Lam _.0))
     (Lam (Lam (App (Lam (Var (s (s z)))) (Lam _.1))))
     (Lam (Lam (Var (s z)))))
    ((App (Lam (Lam (Lam (Var (s z))))) (Lam _.0))
     (App (Lam (Var z)) (Lam (Lam (Var (s z)))))
     (Lam (Lam (Var (s z)))))
    ((App (Lam (Lam (Lam (Var (s z))))) (Lam _.0))
     (Lam (App (Lam (Var z)) (Lam (Var (s z)))))
     (Lam (Lam (Var (s z)))))))

(test "nfo-5"
  (run 5 (e1 e2 ne)
    (=/= e1 e2)
    (=/= e1 ne)
    (=/= e2 ne)
    (=/= `(Lam (Var z)) ne)
    (=/= `(Lam (Lam (Var z))) ne)
    (=/= `(Lam (Lam (Var (s z)))) ne)
    (nfo '() e1 ne)
    (nfo '() e2 ne))
  '((((App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.0))
      (App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.1))
      (Lam (Lam (Lam (Var z)))))
     (=/= ((_.0 _.1))))
    ((App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.0))
     (Lam (App (Lam (Lam (Lam (Var z)))) (Lam _.1)))
     (Lam (Lam (Lam (Var z)))))
    ((App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.0))
     (Lam (Lam (App (Lam (Lam (Var z))) (Lam _.1))))
     (Lam (Lam (Lam (Var z)))))
    ((App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.0))
     (Lam (Lam (Lam (App (Lam (Var (s z))) (Lam _.1)))))
     (Lam (Lam (Lam (Var z)))))
    ((App (Lam (Lam (Lam (Lam (Var z))))) (Lam _.0))
     (App (Lam (Var z)) (Lam (Lam (Lam (Var z)))))
     (Lam (Lam (Lam (Var z)))))))

#|
;;; WEB -- I claim that this should diverge!
(test "nfo-6"
  (run 1 (e1 e2 ne e)
    (=/= e1 e2)
    (=/= e1 ne)
    (=/= e2 ne)
    (== `(App . ,e) ne)
    (nfo '() e1 ne)
    (nfo '() e2 ne))
  '???)
|#


(test "nfo-8"
  ;; Show these two expressions have the same normal form:
  ;;
  ;; ((lambda (x) (lambda (y) (lambda (z) z))) (lambda (w) w))
  ;; (lambda (a) (lambda (b) b))
  ;;
  (run* (nf1 nf2)
    (nfo 'z '(App (Lam (Lam (Lam (Var z)))) (Lam (Var z))) nf1)
    (nfo 'z '(Lam (Lam (Var z))) nf2))
  '(((Lam (Lam (Var z))) (Lam (Lam (Var z)))))
  )


#|
;;; WEB -- these tests no longer run, since I inlined `appo` in `evalo`.
(test "appo-0"
  (run 3 (f v  val)
    (appo f v val))
  '(((N _.0) _.1 (N (NApp _.0 _.1)))
    ((Clo _.0 (Lam _.1)) _.2 (Clo (_.2 . _.0) _.1))
    ((Clo _.0 (Var z)) _.1 _.1)))

(test "appo-1"
  (run 1 (f v)
    (appo f v '(Clo () (Var z))))
  '(((Clo _.0 (Var z)) (Clo () (Var z)))))

(test "appo-2"
  (run 3 (f v n)
    (appo f v `(N (NApp ,n ,v))))
  '(((N _.0) _.1 _.0)
    ((Clo ((N (NApp _.0 _.1)) . _.2) (Var (s z))) _.1 _.0)
    ((Clo (_.0 (N (NApp _.1 _.2)) . _.3) (Var (s (s z)))) _.2 _.1)))

(test "appo-3"
  (run* (f v n val)
    (== `(N ,n) f)
    (appo f v val))
  '(((N _.0) _.1 _.0 (N (NApp _.0 _.1)))))
|#
