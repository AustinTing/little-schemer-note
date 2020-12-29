(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     ((eq? a (car lat)) (cdr lat))
     (else (rember a (cdr lat))))))

(rember 'bacon '(bacon lettuce and tomato))
(rember 'and '(bacon lettuce and tomato)) 

; The Second Commandment
; Use cons to build lists.
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     (else (cond
	    ((eq? (car lat) a) (cdr lat))
	    (else (cons (car lat)
			(rember a
				(cdr lat)))))))))

(rember 'and '(bacon lettuce and tomato))

; Simplify it.
(define rember
  (lambda (a lat)
    (cond
     ((null? lat) (quote ()))
     ((eq? (car lat) a) (cdr lat))
     (else (cons (car lat)
		 (rember a (cdr lat)))))))

(define firsts
  (lambda (l)
    (cond
     ((null? l) '())
     (else (cons (car (car l))
		 (firsts (cdr l)))))))
(firsts '((a b c) (d e f) (h i j)))
(firsts '((a b c)
	  (d e f)
	  ((h) i j)))
; The Third Commandment
; When building a list, describe the first typical element, and then cons it onto the natural recursion. 
(define insertR
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old)
	(cons old ; What is the difference if using (car lat) to replace old?
	      (cons new (cdr lat))))
       (else (cons (car lat)
		   (insertR new old (cdr lat)))))))))

(insertR 'cr 'c '(a b c d e))

(define insertL
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new lat))
       (else (cons (car lat)
		   (insertL new old (cdr lat)))))))))

(insertL 'cl 'c '(a b c d e))

(define subst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (cdr lat)))
       (else (cons (car lat)
		   (subst new old (cdr lat)))))))))
(subst 'cc 'c '(a b c d e)) 

(define subst2
  (lambda (new o1 o2 lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((or (eq? (car lat) o1)
	    (eq? (car lat) o2))
	(cons new (cdr lat)))
       (else (cons (car lat)
		   (subst2 new o1 o2 (cdr lat)))))))))
(subst2 'ff 'b 'd '(a b c d e))

(define multirember
  (lambda (a lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) a) (multirember a (cdr lat)))
       (else (cons (car lat)
		   (multirember a (cdr lat)))))))))
(multirember 'c '(a b c d e d c b a))

(define multiinsertL
 (lambda (new old lat)
   (cond
    ((null? lat) '())
    (else (cond
	   ((eq? (car lat) old) (cons new
				      (cons old (multiinsertL new old (cdr lat)))))
	   (else (cons (car lat) (multiinsertL new old (cdr lat)))))))))
(multiinsertL 'c1 'c '(a b c d e d c b a))

(define multisubst
  (lambda (new old lat)
    (cond
     ((null? lat) '())
     (else
      (cond
       ((eq? (car lat) old) (cons new (multisubst new old (cdr lat))))
       (else (cons (car lat)
		   (multisubst new old (cdr lat)))))))))
(multisubst 'cc 'c '(a b c d e d c b a))





