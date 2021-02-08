(load "ch08.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(define keep-looking
  (lambda (a sorn lat)
    (cond
     ((member? sorn) (keep-looking a (pick sorn lat) lat))
     (else (eq? sorn a)))))
