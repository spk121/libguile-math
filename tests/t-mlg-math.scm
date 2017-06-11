(use-modules (srfi srfi-64)
	     (mlg math))

(test-begin "t-mlg-math")
(test-equal (add-num-or-false) 0)
(test-equal (add-num-or-false 1) 1)
(test-equal (add-num-or-false #f) #f)
(test-equal (add-num-or-false 1 2) 3)
(test-equal (add-num-or-false 1 #f) #f)
(test-equal (add-num-or-false 1 2 3) 6)

;; Finish the testsuite, and report results.
(test-end "t-mlg-math")
