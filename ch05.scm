(load "ch04.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

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

(define insertL*
  (lambda (new old l)
    (cond
     ((null? l) '())
     ((atom? (car l)) (cond
		       ((eq? old (car l)) (cons new (cons old (insertL* new old (cdr l)))))
		       (else (cons (car l) (insertL* new old (cdr l))))))
     (else (cons (insertL* new old (car l)) (insertL* new old (cdr l)))))))

(insertL* 'peaker 'chuck '((how much (wood))
			  could
			  ((a (wood) chuck))
			  (((chuck)))
			  (if (a) ((wood chuck)))
			  could chuck wood))
(define member*
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((atom? (car l)) (or (eq? a (car l))
			  (member* a (cdr l))))
     (else (or (member* a (car l))
	       (member* a (cdr l)))))))
(member* 'chips '((potato) (chips ((with) fish) (chips))))

(define leftmost
  (lambda (l)
    (cond
     ((atom? (car l)) (car l))
     (else (leftmost (car l))))))

(leftmost '((potato) (chips ((with) fish) (chips))))
(leftmost '(((hot) (tuna (and))) cheese))

(define eqlist?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((and (atom? (car l1)) (atom? (car l2))) (and
					       (eqan? (car l1) (car l2))
					       (eqlist? (cdr l1) (cdr l2))))
     ((and (list? (car l1)) (list? (car l2))) (and
					       (eqlist? (car l1) (car l2))
					       (eqlist? (cdr l1) (cdr l2))))
     (else #f))))

(eqlist? '(a b c) '(a b c))
(eqlist? '(a b c) '(a b b))
(eqlist? '(a ((b))) '(a ((b))))
(eqlist? '(a ((b))) '(a ((c))))
(eqlist? '(a ((b)) (c (d))) '(a ((b)) (c (d))))
(eqlist? '(a ((b)) (c (d))) '(a ((d)) (c (d))))

 
					       
