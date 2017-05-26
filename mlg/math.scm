(define-module (mlg math)
  #:use-module (srfi srfi-1)
  #:export (array-absolute-sum-of-slice
            array-rotate-slice-pairs!
            array-scale-slice!
            array-scale-and-add-slice-to-slice!
            array-sum-product-of-slice-pairs
            cast-int32-to-uint32
            cast-uint32-to-int32
            deal
            lognot-uint8
            lognot-uint16
            lognot-uint32
            lognot-uint64
            pythag
            real->integer))

(define (array-absolute-sum-of-slice arr n idx)
  "Given an array ARR, consider the array slice where the index of the
Nth dimension is equal to IDX.  Compute the sum of the absolute values
of the entries of the slice."
  (error 'not-implemented))

(define (array-rotate-slice-pairs! arr n idx1 idx2 theta)
  "Given an array ARR, consider two array slices A and B where the Nth
dimension is equal to IDX1 or IDX2 respectively.
Replace each element of A with cos(theta)*A+sin(theta)*B.
Replace each element of B with -sin(theta)*A+cos(theta)*B."
  (error 'not-implemented))

(define (array-scale-entry! arr scale . indices)
  "Modify array ARR such that the entry at the location given
by indices is multiplied by the scale factor SCALE."
  (apply array-set!
         (append (list arr)
                 (list (* scale (apply array-ref (append (list arr) indices))))
                 indices)))

(define (array-scale-slice! arr dimension idx scale)
  "Given an array ARR, multiply by a scale factor all the elements of
the array where the array index for dimension DIMENSION is equal to
INDEX."
  (array-index-map! arr
                    (lambda indices
                      (if (= idx (list-ref indices dimension))
                          (* (apply array-ref (pk (append (list arr) indices))) scale)
                          ;; else
                          (apply array-ref (append (list arr) indices))))))

(define (array-scale-and-add-slice-to-slice! arr n idx1 idx2 scale)
  "Given an array ARR, consider two array slices, where the Nth array
index is equal to IDX1 and IDX2 respectively.  For each element in the
slice at IDX1, multiply the corresponding element in the slice at IDX2
by the scale factor SCALE, and then add it to element in the slice at
IDX1."
  (array-index-map! arr
                    (lambda indices
                      (if (= idx1 (list-ref indices n))
                          (let ((indices2 (list-copy indices)))
                            (list-set! indices2 n idx2)
                            (+
                             (* (apply array-ref (pk (append (list arr) indices2)))
                                scale)
                             (apply array-ref (append (list arr) indices))))

                          ;; else
                          (apply array-ref (append (list arr) indices))))))

(define (array-sum-product-of-slices arr n idx1 idx2)
  "Given an array ARR, consider two array slices, where the Nth
array index is equal to IDX1 and IDX2 respectively.  Compute the sum
of the the products of the elements of two array slices."
  ;; FIXME: not implementsed
  (error 'not-implemented))

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

(define (pythag x y)
  (sqrt (+ (* x x) (* y y))))

(load-extension "libguile-mlg" "init_math_lib")

;; Local Variables:
;; coding: us-ascii
;; indent-tabs-mode: nil
;; End:
