(load "ch02.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(load "ch03.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))


(load "ch06.scm"
  (lambda (x)
    (pretty-print
      (if (annotation? x)
          (annotation-stripped x)
          x))
    (newline)
    (eval x)))

(define set?
  (lambda (lat)
    (cond
     ((null? lat) #t)
     ((member? (car lat) (cdr lat)) #f)
     (else (set? (cdr lat))))))

(set? '(a a))
(set? '(a b c d))
(set? '(a b c c))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     ((member? (car lat) (cdr lat)) (makeset (cdr lat)))
     (else (cons (car lat)
		 (makeset (cdr lat)))))))

(makeset '(a a))
(makeset '(a b c d))
(makeset '(a b c c))

(define makeset
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cons (car lat)
		 (makeset (multirember (car lat) (cdr lat))))))))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (cond
	    ((member? (car set1) set2) (subset? (cdr set1) set2))
	    (else #f))))))

(member? 'a '(a b))

(define subset?
  (lambda (set1 set2)
    (cond
     ((null? set1) #t)
     (else (and (member? (car set1) set2) (subset? (cdr set1) set2))))))

(subset? '(a b) '(a c d))
(subset? '(a b) '(c b a))

(define eqset?
  (lambda (set1 set2)
    (and (subset? set1 set2) (subset? set2 set1))))

(eqset? '(a b c) '(c b a))
(eqset? '(a b c) '(c d a))

(define intersect?
  (lambda (set1 set2)
    (cond
     ((null? set1) #f)
     (else
      (or (member? (car set1) set2)
	  (intersect? (cdr set1) set2))))))

(intersect? '(a b c) '(c d f))
(intersect? '(a b c) '(e f g))

(define intersect
  (lambda (set1 set2)
    (cond
     ((null? set1) '())
     ((member? (car set1) set2) (cons (car set1)
				      (intersect (cdr set1) set2)))
     (else (intersect (cdr set1) set2)))))

(intersect '(a b c) '(c d f))
(intersect '(a b c) '(e f g))

(define union
  (lambda (set1 set2)
    (cond
     ((null? set1) set2)
     ((member? (car set1) set2) (union (cdr set1) set2))
     (else (cons (car set1) (union (cdr set1) set2))))))

(union '(a b c) '(c d f))
(union '(a b c) '(e f g a b c))

(define intersectall
  (lambda (l-set)
    (cond
     ((null? (cdr l-set)) (car l-set))
     (else (intersect (car l-set) (intersectall (cdr l-set)))))))

(intersectall '((a b c) (b c d) (c d e) (f g c)))

(define a-pair?
  (lambda (x)
    (cond
     ((atom? x) #f)
     ((null? x) #f)
     ((null? (cdr x)) #f) ; False if just only one.
     ((null? (cdr (cdr x))) #t) ; True if there is just two s.
     (else #f))))

(a-pair? '(a 1))
(a-pair? '((a) b))

(define fun?
  (lambda (rel)
    (set? (firsts rel))))

(fun? '((a a) (b b) (c c)))
(fun? '((a a) (b b) (a c)))
