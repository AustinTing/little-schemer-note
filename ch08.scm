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
     ((test? a (car l)) (cdr l))
     (else (cons (car l) ((rember-f test? a (cdr l))))))))

(define rember-f
  (lambda (test?)
    (lambda (a l)
      (cond
       ((null? l) '())
       ((test? a (car l)) (cdr l))
       (else (cons (car l) ((rember-f test?) a (cdr l))))))))

((rember-f equal?) 'a '(c b a))

(define insertL-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l)) (cons new (cons old (cdr l))))
       (else (cons (car l) ((insertL-f test?) new old (cdr l))))))))

((insertL-f equal?) 'L 'b '(a b c d))

(define insertR-f
  (lambda (test?)
    (lambda (new old l)
      (cond
       ((null? l) '())
       ((test? old (car l))  (cons old (cons new (cdr l))))
       (else (cons (car l) ((insertR-f test?) new old (cdr l))))))))

((insertR-f equal?) 'R 'b '(a b c d))


