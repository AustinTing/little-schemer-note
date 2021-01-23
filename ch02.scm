(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define lat?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((atom? (car l)) (lat? (cdr l)))
     (else #f)))) ; A list can be empty, can have an atom in the first position, or can have a list in the first position.

(define member?
  (lambda (a lat)
    (cond
     ((null? lat) #f )
     (else (or (eq? (car lat) a)
	       (member? a (cdr lat)))))))

; The First Commandment
; Always ask null? as the first question in expressing any function.
