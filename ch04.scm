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

(define my-eq?
  (lambda (n m)
    (cond
     ((or (bigger? n m) (smaller? n m)) #f)
     (else #t))))

(my-eq? 2 5)
(my-eq? 5 2)
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

