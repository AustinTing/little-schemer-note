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
