;;; -*- mode: scheme; coding: utf-8; indent-tabs-mode: nil; -*-
;;; (mlg math) - some math and math-like procedures
;;; Copyright (C) 2017 Michael L. Gran <spk121@yahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundataion, either version 3 of
;;; this License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>

(define-module (mlg math)
  #:use-module (srfi srfi-1)
  #:export (
            add-num-or-false
            array-absolute-sum-of-slice
            array-rotate-slice-pairs!
            array-scale-and-add-slice-to-slice!
            array-scale-slice!
            array-sum-product-of-slice-pairs
            binomial-coefficient
            cast-int32-to-uint32
            cast-uint32-to-int32
            cumulative-sum
            dct-f64-forward-8-point
            dct-f64-inverse-8-point
            deal
            gauss-legendre-quadrature
            legendre-polynomial
            lognot-uint16
            lognot-uint32
            lognot-uint64
            lognot-uint8
            make-2d-f32-array
            monotonic-list-pos-to-coord
            pythag
            quadratic-roots
            real->integer

            math-load-extension
            ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper funcs

(define (bytes-to-bits b)
  (* 8 b))

(define (unsigned-limit b)
  (1- (expt 2 (bytes-to-bits b))))

(define (lognot-uint x b)
  (- (unsigned-limit b) (logand (unsigned-limit b) x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;

(define (add-num-or-false . vals)
  "Add the parameters, returning #f if any of the parameters
is not a number."
  (if (null? vals)
      0
      (let loop ((sum 0)
                 (cur (car vals))
                 (rest (cdr vals)))
        (if (not (number? cur))
            #f
            ;; else
            (if (null? rest)
                (+ cur sum)
                ;; else
                (loop (+ cur sum)
                      (car rest)
                      (cdr rest)))))))


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

(define (binomial-coefficient n k)
  "Computes the binomial coefficient 'n choose k'."
  (if (> k n)
      0
      (if (or (= k 0) (= k n))
          1
          ;; else
          (let loop ((val 1)
                     (n n)
                     (k k))
            (if (= k 0)
                val
                ;; else
                (loop (* val (/ n k))
                      (1- n)
                      (1- k)))))))

(define (cast-int32-to-uint32 x)
  (if (< x 0)
      (- #x100000000 (logand #x7fffffff (abs x)))
      (logand #x7FFFFFFF x)))

(define (cast-uint32-to-int32 x)
  (if (<= x #x7fffffff)
      x
      (- (- #x100000000 (logand x #xffffffff)))))

(define (cumulative-sum lst)
  "Given a list of numbers (x0 x1 x2 ...),
 returns a list of the same length of the form
 (x0 x0+x1 x0+x1+x2 ..."
  (if (null? lst)
      lst
      ;; else
      (reverse
       (fold (lambda (cur prev)
               (append (list (+ cur (first prev))) prev))
             (list (car lst))
             (cdr lst)))))

;; The cosine basis function scale factors for the DCT.
(define CU_0 (/ 1.0 (sqrt 2.0)))
(define CU_N 1.0)
(define π 3.141592654)

(define (dct-f64-forward-8-point f)
  "Given a uniform f64vector of 8 numbers, this procedure returns a
uniform f64vector of 8 real numbers which are the coefficients of an
8-point discrete cosine transform."
  (let ((F (make-f64vector 8 0.0)))
    (do ((μ 0 (1+ μ))) ((>= μ 8))
      (let ((coef (if (zero? μ)
                      (* 0.5 CU_0)
                      (* 0.5 CU_N))))
        (do ((x 0 (1+ x))) ((>= x 8))
          (f64vector-set! F μ
                          (+ (f64vector-ref F μ)
                             (* coef
                                (f64vector-ref f x)
                                (cos (/ (* μ π (+ 1.0 (* 2.0 x)))
                                        16.0))))))))
    F))

(define (dct-f64-inverse-8-point F)
  (let ((f (make-f64vector 8 0.0)))
    (do ((x 0 (1+ x))) ((>= x 8))
      (do ((μ 0 (1+ μ))) ((>= μ 8))
        (let ((coef (if (zero? μ)
                        (* 0.5 CU_0)
                        (* 0.5 CU_N))))
          (f64vector-set! f x
                          (+ (f64vector-ref f x)
                             (* coef
                                (f64vector-ref F μ)
                                (cos (/ (* μ π (+ 1.0 (* 2.0 x)))
                                        16.0))))))))
    f))

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


(define (gauss-legendre-quadrature proc n)
  "Integrate PROC, a procedure that maps a number to a number,
over the range -1 to 1, using a Nth order approximation, where
2 <= N <= 6"
  (let ((nodes/weights '(;; n = 2
                         ((-0.5773502692 . 1.0000000000)
                          ( 0.5773502692 . 1.0000000000))
                         ;; n = 3
                         ((-0.7745966692 . 0.5555555556)
                          ( 0.0000000000 . 0.8888888889)
                          ( 0.7745966692 . 0.5555555556))
                         ;; n = 4
                         ((-0.8611363316 . 0.3478588451)
                          ( 0.3399810436 . 0.6521451549)
                          ( 0.8611363316 . 0.3478588451))
                         ;; n = 5
                         ((-0.9061797459 . 0.2369268851)
                          (-0.5384693101 . 0.4786286705)
                          ( 0.0000000000 . 0.5688888889)
                          ( 0.5384693101 . 0.4786286705)
                          ( 0.9061797459 . 0.2369268851))
                         ;; n = 6
                         ((-0.9324695142 . 0.1713244924)
                          (-0.6612093865 . 0.3607615730)
                          (-0.2386191861 . 0.4679139346)
                          ( 0.2386191861 . 0.4679139346)
                          ( 0.6612093865 . 0.3607615730)
                          ( 0.9324695142 . 0.1713244924)))))
    (let ((nw (list-ref nodes/weights (- n 2))))
      (let loop ((i 0)
                 (sum 0.0))
        (if (< i n)
            (let ((nw-cur (list-ref nw i)))
              (format #t "nw-cur ~s nw ~s i ~s ~%" nw-cur nw i)
              (loop (1+ i)
                    (+ sum (* (cdr nw-cur)
                              (proc (car nw-cur))))))
            ;; else
            sum)))))

(define (legendre-polynomial n x)
  "Computes the nth order Legendre polynomial at the location
x, where x is [-1, 1]."
  ;; Using the explicit expression
  ;; P_n(x) = (1 / 2^n) * SUM_0^floor(n/2) -1^m (binom n m) (binom 2n-2m n) x&(n-2m
  ;;
  (cond
   ((= n 0)
    1)
   ((= n 1)
    x)
   ((= n 2)
    (* 1/2 (+ (* 3 x x) -1)))
   ((= n 3)
    (* 1/2 (+ (* 5 x x x) (* -3 x))))
   ((= n 4)
    (* 1/8 (+ (* 35 (expt x 4)) (* -30 x x) 3)))
   (else 
    (let ((A (/ 1 (expt 2 n)))
          (range (floor (/ n 2))))
      (* A
         (let loop ((m 0)
                    (sum 0))
           (if (<= m range)
               (let ((sgn (expt -1 m))
                     (B (binomial-coefficient n m))
                     (C (binomial-coefficient (- (* 2 n) (* 2 m)) n))
                     (D (expt 1 (- n (* 2 m)))))
                 (loop (1+ m)
                       (+ sum (* sgn B C D))))
               ;; else
               sum)))))))

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

(define (make-2d-f32-array height width)
  "Make a standard 2D array for floating point math. It is
initialized to zero."
  (make-typed-array 'f32 0.0 height width))

(define (monotonic-list-pos-to-coord lst x)
  "Given a list of monotonically increasing integers (x1 x2 x3 ...)
this returns a pair.
The first element is
 0 if 0  <= x < x1
 1 if x1 <= x < x2
 2 if x2 <= x < x3, etc.
The second element is the difference between x and the lower limit.

Thus ((list 5 10 15) 7) => (1 2)
 since x1 <= 7 and 7 - x1 = 2"
  (let loop ((j 0)
             (prev 0)
             (cur (car lst))
             (rest (cdr lst)))
    (if (and (<= prev x) (< x cur))
        (list j (- x prev))
        (loop (1+ j) cur (car rest) (cdr rest)))))

(define (pythag x y)
  (sqrt (+ (* x x) (* y y))))

(define (quadratic-roots a b c)
  "Given a quadratic equation Ax^2 + Bx + C = 0, find the roots."
  (if (zero? a)
      (if (zero? b)
          '()
          ;; else
          (list (/ (- c) b)))
      ;; else
      (let* ((det (if (>= b 0)
                      (sqrt (- (* b b) (* 4 a c)))
                      (- (sqrt (- (* b b) (* 4 a c))))))
             (q (* -0.5 (+ b det))))
        (list (/ q a) (/ c q)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; For C-defined functions

(define *math-lib-loaded* #f)
(define (math-load-extension)
  (unless *math-lib-loaded*
    (set! *math-lib-loaded* #t)
    (load-extension "libguile-mlg" "init_math_lib")))
