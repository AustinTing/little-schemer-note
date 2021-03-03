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



     
