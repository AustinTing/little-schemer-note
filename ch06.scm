(load "ch05.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(define numbered
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (car (cdr aexp)))))))))
