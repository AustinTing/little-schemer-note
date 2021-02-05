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

(define multirember-f
  (lambda (test?)
    (lambda (a lat)
      (cond
       ((null? lat) '())
       ((test? a (car lat)) ((multirember-f test?) a (cdr lat)))
       (else (cons (car lat) ((multirember-f test?) a (cdr lat))))))))

((multirember-f equal?) 'b '(a b c b b a))

(define eq-c?
  (lambda (a)
    (lambda (x)
      (eq? x a))))

((eq-c? 'b) 'a)

(define eq-c?-b
  (eq-c? 'b))

(eq-c?-b 'b)
(eq-c?-b 'a)

(define multiremberT
  (lambda (test? lat)
    (cond
     ((null? lat) '())
     ((test? (car lat)) (multiremberT test? (cdr lat)))
     (else (cons (car lat) (multiremberT test? (cdr lat)))))))

(multiremberT eq-c?-b '(a b c b b a))

(define multirember&co
  (lambda (a lat col)
    (cond
     ((null? lat) (col '() '()))
     ((eq? a (car lat)) (multirember&co a (cdr lat) (lambda (newlat seen)
						      (col newlat (cons (car lat) seen)))))
     (else (multirember&co a (cdr lat) (lambda (newlat seen)
					 (col (cons (car lat) newlat) seen)))))))
(define a-friend
  (lambda (x y)
    (null? y)))

(multirember&co 'b '() a-friend)
(multirember&co 'b '(b) a-friend)
;; (multirember&co 'b '(b) a-friend)
;; ...
;; ((eq? 'b 'b) (multirember&co 'b '() (lambda (newlat seen)
;;				        (a-friend newlat (cons 'b seen)))))
;; ...
;; -natural recursion
;; (multirember&co 'b '() (lambda (newlat seen)
;;			    (a-friend newlat (cons 'b seen))))
;; ...
;; ((null? '()) (a-friend '() (cons 'b '()))) ; #f
;; ...

(multirember&co 'b '(a b) a-friend)
;; (multirember&co 'b '(a b) a-friend)
;; ...
;; (else (multirember&co 'b '(b) (lambda (newlat seen)
;; 				(a-friend (cons 'a newlat) seen))))
;; ...
;; -natural recursion
;; (multirember&co 'b '(b) (lambda (newlat seen)
;; 			  (a-friend (cons 'a newlat) seen)))
;; ...
;; ((eq? 'b 'b) (multirember&co 'b '() (lambda (newlat seen)
;; 				      (a-friend (cons 'a newlat) (cons 'b seen)))))
;; ...
;; -natural recursion
;; (multirember&co 'b '() (lambda (newlat seen)
;; 			 (a-friend (cons 'a newlat) (cons 'b seen))))
;; ...
;; ((null? '()) (a-friend (cons 'a '()) (cons 'b '()))) ; #f

(define multiinsertLR
  (lambda (new oldL oldR lat)
    (cond
     ((null? lat) '())
     ((eq? oldL (car lat)) (cons new
				 (cons oldL
				       (multiinsertLR new oldL oldR (cdr lat)))))
     ((eq? oldR (car lat)) (cons oldR
				(cons new
				      (multiinsertLR new oldL oldR (cdr lat)))))
     (else (cons (car lat)
		 (multiinsertLR new oldL oldR (cdr lat)))))))

(multiinsertLR 'n 'L 'R '(a b L c d R e f))
