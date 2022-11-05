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
