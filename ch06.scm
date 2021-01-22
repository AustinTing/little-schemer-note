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

















(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +)))
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

















(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +)))
     (load "ch05.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))


(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     ((eq? (car (cdr aexp)) (quote +)) (and (numbered? (car aexp))
					    (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote -)) (and (numbered? (car aexp))
					    (numbered? (car (cdr (cdr aexp))))))
     ((eq? (car (cdr aexp)) (quote x)) (and (numbered? (car aexp))
					    (numbered? (car (cdr (cdr aexp)))))))))
(define numbered?
  (lambda (aexp)
    (cond
     ((atom? aexp) (number? aexp))
     (else
      (and (numbered? (car aexp)) (numbered? (car (cdr (cdr aexp)))))))))

(numbered? '(1 x 1))
(numbered? '(1 x p))

(define value
  (lambda (nexp)
    (cond 
     ((atom? nexp) nexp)
     ((eq? (car nexp) (quote +)) (jia (value (car (cdr nexp)))
				      (value (car (cdr (cdr nexp))))))
     ((eq? (car nexp) (quote x)) (cheng (value (car (cdr nexp)))
					(value (car (cdr (cdr nexp))))))
     (else (up (value (car (cdr nexp)))
	       (value (car (cdr (cdr nexp)))))))))

(up 2 3)

(value '(+ 1 (x 2 3)))

