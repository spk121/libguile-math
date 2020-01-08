(use-modules (srfi srfi-64)
             (mlg math))

(test-begin "t-mlg-math")
(test-equal "add-num-or-false null" (add-num-or-false) 0)
(test-equal "add-num-or-false 1" (add-num-or-false 1) 1)
(test-equal "add-num-or-false #f" (add-num-or-false #f) #f)
(test-equal "add-num-or-false 1 2" (add-num-or-false 1 2) 3)
(test-equal "add-num-or-false 1 #f" (add-num-or-false 1 #f) #f)
(test-equal "add-num-or-false 1 2 3" (add-num-or-false 1 2 3) 6)

(test-equal "binomial-coefficient 1 0" (binomial-coefficient 1 0) 1)
(test-equal "binomial-coefficient 1 1" (binomial-coefficient 1 1) 1)
(test-equal "binomial-coefficient 13 0" (binomial-coefficient 13 0) 1)
(test-equal "binomial-coefficient 13 1" (binomial-coefficient 13 1) 13)
(test-equal "binomial-coefficient 13 2" (binomial-coefficient 13 2) 78)
(test-equal "binomial-coefficient 13 3" (binomial-coefficient 13 3) 286)

;; Finish the testsuite, and report results.
(test-end "t-mlg-math")
