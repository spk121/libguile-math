(define-module (mlg typechecking)
  #:use-module (srfi srfi-1)
  #:export (integer-nonnegative?
	    list-of-integers?
	    list-of-two-integers?
	    list-of-strings?
	    list-length-1?
	    list-length-2?))

(define (integer-nonnegative? x)
  (and (integer? x)
       (>= x 0)))

(define (list-of-integers? x)
  "Return #t if x is either an empty list or a list of integers."
  (and (list? x)
       (or (null? x)
	   (every integer? x))))

(define (list-of-two-integers? x)
  "Return #t if x is either an empty list or a list of integers."
  (and (list? x)
       (= 2 (length x))
       (every integer? x)))

(define (list-of-strings? x)
  "Return #t if x is either an empty list of a list of strings."
  (and (list? x)
       (or (null? x)
	   (every string? x))))

(define (list-length-1? x)
  "Return #t if x is a list with a single entry."
  (and (list? x)
       (= 1 (length x))))

(define (list-length-2? x)
  "Return #t if x is a list with a two entries."
  (and (list? x)
       (= 2 (length x))))

