(load "ch08.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(define keep-looking
  (lambda (a n-th lat)
    (cond
     ((number? n-th) (keep-looking a (pick n-th lat) lat))
     (else (eq? n-th a)))))

(define looking
  (lambda (a lat)
    (keep-looking a (pick 1 lat) lat)))

(looking 'caviar '(6 2 4 caviar 5 7 3))
(looking 'caviar '(6 2 grits caviar 5 7 3))

(define shift
  (lambda (l)
    (build (first (first l))
	   (build (second (first l))
		  (second l)))))

(shift '((a b) (c d)))

(define length*
  (lambda (l)
    (cond
     ((atom? l) 1)
     (else (jia (length* (first l))
		(length* (second l)))))))

(length* '((a b) (c d)))

