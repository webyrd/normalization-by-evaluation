(load "nbe-extended.scm")
(load "../../scheme-helpers/test-macro.scm")

(test "nfo-#f"
  (run* (q)
    (nfo '() (parse '#f) q))
  '(#f))

(test "nfo-#t"
  (run* (q)
    (nfo '() (parse '#t) q))
  '(#t))

(test "nfo-if-0"
  (run* (q)
    (nfo '() (parse '(if #f 5 6)) q))
  '(6))

(test "nfo-if-1"
  (run* (q)
    (nfo '() (parse '(if #t 5 6)) q))
  '(5))

(test "nfo-if-2"
  (run* (q)
    (nfo '() (parse '(if (if #t #f #t) 5 6)) q))
  '(6))

(test "nfo-if-3"
  (run* (q)
    (nfo '() (parse '(if (if #f #f #t) 5 6)) q))
  '(5))

(test "nfo-if-4"
  (run* (q)
    (nfo '() (parse '(if (if #f #f #t) (if #f 5 6) (if #t 7 8))) q))
  '(6))

(test "nfo-if-5"
  (run* (q)
    (nfo '() (parse '(if (if #t #f #t) (if #f 5 6) (if #t 7 8))) q))
  '(7))

(test "nfo-if-6"
  (run* (q)
    (nfo '() (parse '(lambda (x) (if x (if #f 5 6) (if #t 7 8)))) q))
  '((Lam (if (Var z) 6 7))))
