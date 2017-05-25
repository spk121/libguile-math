(define-module (mlg math)
  #:use-module (srfi srfi-1)
  #:export (cast-int32-to-uint32
	    cast-uint32-to-int32
	    deal
	    lognot-uint8
	    lognot-uint16
	    lognot-uint32
	    lognot-uint64
	    real->integer))

(define (cast-int32-to-uint32 x)
  (if (< x 0)
      (- #x100000000 (logand #x7fffffff (abs x)))
      (logand #x7FFFFFFF x)))

(define (cast-uint32-to-int32 x)
  (if (<= x #x7fffffff)
      x
      (- (- #x100000000 (logand x #xffffffff)))))

(define (deal n low high)
  "Return a list of N distinct integers with values between
LOW (inclusive) and HIGH (exclusive)."
  (let loop ((i 0)
	     (lst (map (lambda (x) (+ x low)) (iota (- high low))))
	     (out '()))
    (if (>= i n)
	out
	(let ((j (random (length lst))))
	  (loop (1+ i)
		(append (take lst j) (drop lst (1+ j)))
		(append out (list (list-ref lst j))))))))

(define (lognot-uint8 x)
  "Find the bitwise complement of an 8-bit unsigned integer."
  (lognot-uint x 1))

(define (lognot-uint16 x)
  "Find the bitwise complement of a 16-bit unsigned integer."
  (lognot-uint x 2))

(define (lognot-uint32 x)
  "Find the bitwise complement of a 32-bit unsigned integer."
  (lognot-uint x 4))

(define (lognot-uint64 x)
  "Find the bitwise complement of a 64-bit unsigned integer."
  (lognot-uint x 8))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This is all from (ryu core)

(define (bytes-to-bits b)
  (* 8 b))

(define (signed-limit b)
  (1- (expt 2 (1- (bytes-to-bits b)))))

(define (unsigned-limit b)
  (1- (expt 2 (bytes-to-bits b))))

(define (signed-limit-neg b)
  (- (expt 2 (1- (bytes-to-bits b)))))

(define (lognot-uint x b)
  (- (unsigned-limit b) (logand (unsigned-limit b) x)))

(load-extension "libguile-mlg" "init_math_lib")
