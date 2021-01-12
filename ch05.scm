(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define rember*
  (lambda (a l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		      ((eq? a (car l)) (rember* a (cdr l)))
		      (else (cons (car l) (rember* a (cdr l))))))
     (else (cons (rember* a (car l)) (rember* a (cdr l)))))))

(rember* 'cup '((coffee) cup ((tea) cup) (and (hick))))

; A list can be empty, can have an atom in the first position, or can have a list in the first position.
(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f))))

(define insertR*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((eq? old (car l))  (cons (car l) (cons new (insertR* new old (cdr l)))))
		       (else (cons (car l) (insertR* new old (cdr l))))))
     (else (cons (insertR* new old (car l)) (insertR* new old (cdr l)))))))

(insertR* 'roast 'chuck '((how much (wood))
			  could
			  ((a (wood) chuck))
			  (((chuck)))
			  (if (a) ((wood chuck)))
			  could chuck wood))

(define add1
  (lambda (n)
    (+ 1 n)))

(define jia ;; +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (jia n (sub1 m)))))))


(define occur*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((atom? (car l)) (cond
		       ((eq? a (car l)) (add1 (occur* a (cdr l))))
		       (else (occur* a (cdr l)))))
     (else (jia (occur* a (car l)) (occur* a (cdr l)))))))

(occur* 'banana '((banana)
		  (split ((((banana ice)))
			  (cream (banana))
			  sherbet))
		  (banana)
		  (bread)
		  (banana brandy)))

(define subst*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		      ((eq? old (car l)) (cons new (subst* new old (cdr l))))
		      (else (cons (car l) (subst* new old (cdr l))))))
     (else (cons (subst* new old (car l)) (subst* new old (cdr l)))))))

(subst* 'orange 'banana '((banana)
			  (split ((((banana ice)))
				  (cream (banana))
				  sherbet))
			  (banana)
			  (bread)
			  (banana brandy)))

		
