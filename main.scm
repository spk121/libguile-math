(use-modules (chop))

(define T1 (gettimeofday))
(define nums (map (lambda (x) (* 1.0 x)) (iota 1000000)))

(define T2 (gettimeofday))
(define A (map (lambda (x) (inexact->exact x)) nums))

(define T3 (gettimeofday))
(define B (map (lambda (x) (inexact->exact (round x))) nums))

(define T4 (gettimeofday))
;;(define C (map (lambda (x) (numerator (rationalize x 0))) nums))
(define C (map real->small-integer nums))
(define T5 (gettimeofday))


(write (- (+ (* 1000000 (car T2)) (cdr T2)) (+ (* 1000000 (car T1)) (cdr T1)))) (newline)
(write (- (+ (* 1000000 (car T3)) (cdr T3)) (+ (* 1000000 (car T2)) (cdr T2)))) (newline)
(write (- (+ (* 1000000 (car T4)) (cdr T4)) (+ (* 1000000 (car T3)) (cdr T3)))) (newline)
(write (- (+ (* 1000000 (car T5)) (cdr T5)) (+ (* 1000000 (car T4)) (cdr T4)))) (newline)
