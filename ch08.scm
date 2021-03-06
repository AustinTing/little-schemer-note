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

(define multiinsertLR&co
  (lambda (new oldL oldR lat col)
    (cond
     ((null? lat) (col '() 0 0))
     ((eq? oldL (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
					     (lambda (newlat numL numR)
					       (col (cons new
							  (cons oldL newlat))
						    (add1 numL)
						    numR))))
     ((eq? oldR (car lat)) (multiinsertLR&co new oldL oldR (cdr lat)
					     (lambda (newlat numL numR)
					       (col (cons oldR
							  (cons new newlat))
						    numL
						    (add1 numR)))))
     (else (multiinsertLR&co new oldL oldR (cdr lat)
			     (lambda (newlat numL numR)
			       (col (cons (car lat) newlat) numL numR)))))))

(multiinsertLR&co 'n 'L 'R '(a b L c d R e f) (lambda (newlat numL numR)
						(cons newlat (cons numL (cons numR '())))))

(define even?
  (lambda (n)
    (= (* (quotient n 2) 2) n)))

(even? 7)

(define evens-only*
  (lambda (l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((even? (car l)) (cons (car l)
					      (evens-only* (cdr l))))
		       (else (evens-only* (cdr l)))))
     (else (cons (evens-only* (car l))
		 (evens-only* (cdr l)))))))

(evens-only* '(1 2 3 4 5))
(evens-only* '(1 (2 3 (4 5 (6 7))) (8 9)))

(define third
  (lambda (l)
    (car (cdr (cdr l)))))
(third '(a b c))

(define evens-only*&co
  (lambda (l col)
    (cond
     ((null? l) (col '() 1 0))
     ((atom? (car l)) (cond
		       ((even? (car l)) (evens-only*&co (cdr l) (lambda (newlat product sum)
								  (col (cons (car l) newlat)
								       (cheng (car l) product)
								       sum))))
		       (else (evens-only*&co (cdr l) (lambda (newlat product sum)
						       (col newlat
							    product
							    (jia (car l) sum)))))))
     (else (evens-only*&co (car l) (lambda (car-newlat car-product car-sum)
				     (evens-only*&co (cdr l) (lambda (cdr-newlat cdr-product cdr-sum)
							       (col (cons car-newlat cdr-newlat)
								    (cheng car-product cdr-product)
								    (jia car-sum cdr-sum))))))))))

(define cons-result
  (lambda (newlat product sum)
    (cons newlat (cons product (cons sum '())))))

(evens-only*&co '() cons-result)
(evens-only*&co '(1) cons-result)
(evens-only*&co '(2 4) cons-result)
(evens-only*&co '(2 3 4 5) cons-result)
(evens-only*&co '((1 2) 3 4) cons-result)
(evens-only*&co '(1 (2 3 (4 5 (6 7))) (8 9)) cons-result)
;; (evens-only*&co '((1 2) 3 4) cons-result)
;; ... (car l) is (1 2), (cdr l) is (3 4), col is cons-result.
;; (else (evens-only*&co (1 2) (lambda (car-newlat car-product car-sum)
;; 				     (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 							       (cons-result (cons car-newlat cdr-newlat)
;; 									    (cheng car-product cdr-product)
;; 									    (jia car-sum cdr-sum)))))))
;; ...
;; -
;; (evens-only*&co (1 2) (lambda (car-newlat car-product car-sum)
;; 			(evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 						(cons-result (cons car-newlat cdr-newlat)
;; 							     (cheng car-product cdr-product)
;; 							     (jia car-sum cdr-sum))))))
;; ... (car l) is 1, (cdr l) is (2), col is like above.
;; (else (evens-only*&co (2) (lambda (newlat product sum)
;; 			    (col newlat
;; 				 product
;; 				 (jia 1 sum)))))
;; ... replace col
;; (else (evens-only*&co (2) (lambda (newlat product sum)
;; 			    (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 						    (cons-result (cons newlat cdr-newlat)
;; 								 (cheng product cdr-product)
;; 								 (jia (jia 1 sum) cdr-sum)))))))
;; ...
;; -
;; (evens-only*&co (2) (lambda (newlat product sum)
;; 			  (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 						  (cons-result (cons newlat cdr-newlat)
;; 							       (cheng product cdr-product)
;; 							       (jia (jia 1 sum) cdr-sum))))))
;; ...(car l) is 2, (cdr l) is (), col is like above.
;; ((even? 2) (evens-only*&co () (lambda (newlat product sum)
;; 				(col (cons 2 newlat)
;; 				     (cheng 2 product)
;; 				     sum))))
;; ... replace col
;; ((even? 2) (evens-only*&co () (lambda (newlat product sum)
;; 				(evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 							 (cons-result (cons (cons 2 newlat) cdr-newlat)
;; 								      (cheng (cheng 2 product) cdr-product)
;; 								      (jia (jia 1 sum) cdr-sum)))))))
;; ...
;; -
;; (evens-only*&co () (lambda (newlat product sum)
;; 		     (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 					     (cons-result (cons (cons 2 newlat) cdr-newlat)
;; 							  (cheng (cheng 2 product) cdr-product)
;; 							  (jia (jia 1 sum) cdr-sum))))))
;; ... l is (), col is like above.
;; ((null? ()) (col '() 1 0))
;; ... replace col
;; ((null? ()) (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 					     (cons-result (cons (cons 2 '()) cdr-newlat)
;; 							  (cheng (cheng 2 1) cdr-product)
;; 							  (jia (jia 1 0) cdr-sum)))))
;; ...
;; -
;; (evens-only*&co (3 4) (lambda (cdr-newlat cdr-product cdr-sum)
;; 					     (cons-result (cons (cons 2 '()) cdr-newlat)
;; 							  (cheng (cheng 2 1) cdr-product)
;; 							  (jia (jia 1 0) cdr-sum))))
;; ...(car l) is 3, (cdr l) is (4). col is like above.
;; (else (evens-only*&co (4) (lambda (newlat product sum)
;; 			    (col newlat
;; 				 product
;; 				 (jia 3 sum)))))
;; ... replace col
;; (else (evens-only*&co (4) (lambda (newlat product sum)
;; 			    (cons-result (cons (cons 2 '()) newlat)
;; 					 (cheng (cheng 2 1) product)
;; 					 (jia (jia 1 0) (jia 3 sum))))))
;; ...
;; -
;; (evens-only*&co (4) (lambda (newlat product sum)
;; 			    (cons-result (cons (cons 2 '()) newlat)
;; 					 (cheng (cheng 2 1) product)
;; 					 (jia (jia 1 0) (jia 3 sum)))))
;; ...(car l) is 4, (cdr l) is (). col is like above.
;; ((even? 4) (evens-only*&co () (lambda (newlat product sum)
;; 				(col (cons 4 newlat)
;; 				     (cheng 4 product)
;; 				     sum))))
;; ... replace col
;; ((even? 4) (evens-only*&co () (lambda (newlat product sum)
;; 				(cons-result (cons (cons 2 '()) (cons 4 newlat))
;; 					     (cheng (cheng 2 1) (cheng 4 product))
;; 					     (jia (jia 1 0) (jia 3 sum))))))
;; ...
;; -
;; (evens-only*&co () (lambda (newlat product sum)
;; 				(cons-result (cons (cons 2 '()) (cons 4 newlat))
;; 					     (cheng (cheng 2 1) (cheng 4 product))
;; 					     (jia (jia 1 0) (jia 3 sum)))))
;; ... l is (). col is like above.
;; ((null? ()) (col '() 1 0))
;; ... replace col
;; ((null? ()) (cons-result (cons (cons 2 '()) (cons 4 '()))
;; 			 (cheng (cheng 2 1) (cheng 4 1))
;; 			 (jia (jia 1 0) (jia 3 0))))
;; ...
