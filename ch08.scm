(load "ch07.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(define rember-f
  (lambda (test? a l)
    (cond
     ((null? l) '())
     ((test? a (car l)) (rember-f test? a (cdr l)))
     (else (cons (car l) ((rember-f test? a (cdr l))))))))

