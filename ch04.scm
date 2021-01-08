(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x)))))

(define add1
  (lambda (n)
    (+ 1 n)))

(add1 3)

(define sub1
  (lambda (n)
    (- n 1)))

(sub1 5)

(define jia ;; +
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (add1 (jia n (sub1 m)))))))

(jia 1 2) ;; => 3

(define jian ;; -
  (lambda (n m)
    (cond
     ((zero? m) n)
     (else (sub1 (jian n (sub1 m)))))))
(jian 9 2) ;; => 7
(jian 9 10) ;; => -1

(define addtup
  (lambda (tup)
    (cond
     ((null? tup) 0)
     (else (jia (car tup) (addtup (cdr tup)))))))

(addtup '(1 2 3 4))

(define cheng ;; x
  (lambda (n m)
    (cond
     ((zero? m) 0)
     (else (jia n (cheng n (sub1 m)))))))

(cheng 12 3)

(define tup+
  (lambda (tup1 tup2)
    (cond
     ((null? tup1) tup2)
     ((null? tup2) tup1)
     (else (cons (jia (car tup1) (car tup2)) (tup+ (cdr tup1) (cdr tup2)))))))
(tup+ '(1 2 3 4)
      '(5 6 7 8 9 10))

(define bigger? ;; >
  (lambda (n m)
    (cond
     ((zero? n) #f)
     ((zero? m) #t)
     (else (bigger? (sub1 n) (sub1 m))))))

(bigger? 2 5)
(bigger? 5 3)
(bigger? 5 5)

(define smaller?
  (lambda (n m)
    (cond
     ((zero? m) #f)
     ((zero? n) #t)
     (else (smaller? (sub1 n) (sub1 m))))))

(smaller? 2 5)
(smaller? 5 3)
(smaller? 5 5)

(define num-eq?
  (lambda (n m)
    (cond
     ((or (bigger? n m) (smaller? n m)) #f)
     (else #t))))

(num-eq? 2 5)
(num-eq? 5 2)
(my-eq? 5 5)

(define up
  (lambda (n m)
    (cond
     ((zero? m) 1)
     (else (cheng n (up n (sub1 m)))))))

(up 1 2)
(up 2 3)
(up 5 3)

(define divided
  (lambda (n m)
    (cond
     ((smaller? n m) 0)
     (else (add1 (divided (jian n m) m))))))

(divided 15 4)


(define my-length
  (lambda (lat)
    (cond
     ((null? lat) 0)
     (else (add1 (my-length (cdr lat)))))))

(my-length '(1 2 3))

(define pick
  (lambda (n lat)
    (cond
     ;; ((eq? 1 n) (car lat)) Against The First Commandment 
     ((zero? (sub1 n)) (car lat))
     (else (pick (sub1 n) (cdr lat))))))

(pick 2 '(a b c))

(define rempick
  (lambda (n lat)
    (cond
     ((zero? (sub1 n)) (cdr lat))
     (else (cons (car lat) (rempick (sub1 n) (cdr lat)))))))

(rempick 3 '(a b c d e f))
(rempick 4 '(a b c d e f))
(rempick 1 '(a b c d e f))

(number? 1)

;; my self
;; (define no-nums
;;   (lambda (lat)
;;     (cond
;;      ((null? lat) '())
;;      ((number? (car lat)) (no-nums (cdr lat)))
;;      (else (cons (car lat) (no-nums (cdr lat)))))))

(define no-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
	    ((number? (car lat)) (no-nums (cdr lat)))
	    (else (cons (car lat) (no-nums (cdr lat)))))))))


(no-nums '(a 1 b 2 c 3))
(no-nums '(1 2 3 4 5))

(define all-nums
  (lambda (lat)
    (cond
     ((null? lat) '())
     (else (cond
	    ((number? (car lat)) (cons (car lat) (all-nums (cdr lat))))
	    (else (all-nums (cdr lat))))))))

(all-nums '(1 2 3))
(all-nums '(a b c))
(all-nums '(a 2 c))

(define eqan?
  (lambda (a1 a2)
    (cond
     ((and (number? a1) (number? a2) (num-eq? a1 a2)))
     ((or (number? a1) (number? a2)) #f)
     (else (eq? a1 a2)))))
(eqan? 1 2)
(eqan? 1 1)
(eqan? 1 'a)
(eqan? 'a 'a)

(define occur
  (lambda (a lat)
    (cond
     ((null? lat) 0)
     (else (cond
	    ((eqan? a (car lat)) (add1 (occur a (cdr lat))))
	    (else (occur a (cdr lat))))))))

(occur 'a '(1 a 2 b 3 c 4 a 5 b))
(occur 2 '(1 a 2 b 3 c 2 a 5))

;; my self
;; (define one?
;;   (lambda (n)
;;     (eqan? 1 n)))

(define one?
  (lambda (n)
    (cond
     ((zero? n) #f)
     (else (zero? (sub1 n))))))

(one? 1)

(define new-rempick
  (lambda (n lat)
    (cond
     ((one? n) (cdr lat))
     (else (cons (car lat) (new-rempick (sub1 n) (cdr lat)))))))


(new-rempick 3 '(a b c d e f))
(new-rempick 4 '(a b c d e f))
(new-rempick 1 '(a b c d e f))

     
	       

