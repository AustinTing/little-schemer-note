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

(define operator
  (lambda (aexp)
    (car aexp)))

(define 1st-sub-exp
  (lambda (aexp)
    (car (cdr aexp))))

(define 2nd-sub-exp
  (lambda (aexp)
    (car (cdr (cdr aexp)))))

(define value
  (lambda (nexp)
    (cond 
     ((atom? nexp) nexp)
     ((eq? (operator nexp) (quote +)) (jia (value (1st-sub-exp nexp))
					   (value (2nd-sub-exp nexp))))
     ((eq? (operator nexp) (quote x)) (cheng (value (1st-sub-exp nexp))
					     (value (2nd-sub-exp nexp))))
     (else (up (value (1st-sub-exp nexp)))
	       (value (2nd-sub-exp nexp))))))

(up 2 3)

(value '(+ 1 (x 2 3)))



