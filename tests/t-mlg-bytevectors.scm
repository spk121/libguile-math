(use-modules (srfi srfi-64)
	     (rnrs bytevectors)
	     (mlg bytevectors))

(test-begin "t-mlg-bytevectors")

(define f32bv (list->f32vector (list -1.0 0.0 1.0)))
(test-equal 1.0 (f32vector-max f32bv))

;; Finish the testsuite, and report results.
(test-end "t-mlg-bytevectors")
