(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))


(list? '()) ; #t
(atom? '()) ; #f 
; The Law of Car
(car 'a) ; Exception in car: a is not a pair
(car '()) ; Exception in car: () is not a pair
; The Law of Cdr (Could-er)
(cdr '(hamberger)) ; ()
(cdr 'hamberger) ; Exception in cdr: hamberger is not a pair
(cdr '()) ; Exception in cdr: () is not a pair
(cdr (cdr '((b) (x y) ((c))))) ; (((c))) Notice: The cdr of any non-empty list is always another list.
(cons 'peanut '(buter and jelly)) ; (peanut buter and jelly) Notice: Read: "cons the atom a onto the list l."
; The Law of Cons
(null? '()) ; #t
(null? 'e) ; #f No answer. Because you cannot ask null? of an atom. In practice, (null? a) is false for everything, except the empty list.
;  The Law of Null?
(eq? 'Harry 'Harry) ; t
(eq? '() '()) ; t Why no answer??
(eq? 7 7) ; t Why no answer??


