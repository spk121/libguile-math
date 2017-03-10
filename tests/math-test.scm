(use-modules (mlg math))

(define (now)
  (let ((T (gettimeofday)))
    (+ (car T) (/ (cdr T) 1000000.0))))

(define T1 (now))
(define nums (map (lambda (x) (* 1.0 x)) (iota 1000000)))

(define T2 (now))
(define A (map (lambda (x) (inexact->exact x)) nums))

(define T3 (now))
(define B (map (lambda (x) (inexact->exact (round x))) nums))

(define T4 (now))
(define C (map real->integer nums))
(define T5 (now))

(display "(inexact->exact x) ")
(write (- T3 T2)) (newline)

(display "(inexact->exact (round x)) ")
(write (- T4 T3)) (newline)

(display "(real->integer x) ")
(write (- T5 T4)) (newline)
