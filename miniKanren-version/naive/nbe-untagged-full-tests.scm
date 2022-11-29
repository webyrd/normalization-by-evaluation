(load "nbe-untagged-full.scm")
(load "../faster-miniKanren/test-check.scm")

(test "foo-1"
  (run* (expr)
    (nfo '(lambda (x) ((lambda (y) y) 5))
         expr))
  '(((lambda (_.0) 5) (sym _.0))))
