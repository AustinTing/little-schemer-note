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

(define seqL
  (lambda (new old l)
    (cons new (cons old l))))

(seqL 'L 'a '(b c))

(define seqR
  (lambda (new old l)
    (cons old (cons new l))))

(seqR 'R 'a '(b c))

(define insert-g
 (lambda (seq)
   (lambda (new old l)
     (cond
      ((null? l) '())
      ((equal? old (car l)) (seq new old (cdr l)))
      (else (cons (car l) ((insert-g seq) new old (cdr l))))))))

((insert-g seqL) 'L 'b '(a b c))
((insert-g seqR) 'R 'b '(a b c))

(define seqrem
  (lambda (new old l)
    l))

(define yyy
  (lambda (a l)
    ((insert-g seqrem) #f a l)))

(yyy 'b '(a b c))

(define atom-to-function
  (lambda (x)
    (cond
     ((eq? x '+) jia)
     ((eq? x 'x) cheng)
     (else up))))

(eq? 'x (operator '(x 1 2)))

(atom-to-function (operator '(+ 1 2)))

(define value
  (lambda (nexp)
    (cond
     ((atom? nexp) nexp)
     (else ((atom-to-function (operator nexp))
	    (value (1st-sub-exp nexp))
	    (value (2nd-sub-exp nexp)))))))
      
(value '(+ (x 2 3) (^ 3 4)))

    


