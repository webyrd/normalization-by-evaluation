;; Adapted from Oleg Kislyov's test macro
(define-syntax test
  (syntax-rules ()
    ((_ title tested-expression expected-result)
     (begin
       (printf "Testing ~s\n" title)
       (let* ((expected expected-result)
              (produced tested-expression))
         (or (equal? expected produced)
             (printf "Failed: ~a\nExpected: ~a\nComputed: ~a\n"
                     'tested-expression expected produced)))))))
