(use-modules (srfi srfi-64)
             (mlg lists))

(test-begin "t-mlg-lists")
(define BigL (iota 10000))
(define MediumL (iota 100))
(define SmallL (iota 1))

(test-assert "list-range-check-n? '() 10 0 10" (list-range-check-n? '() 10 0 10))
(test-assert "list-range-check-n? MediumL 3 0 2" (list-range-check-n? MediumL 3 0 2))
(test-assert "list-range-check-n? MediumL 3 3 6" (not (list-range-check-n? MediumL 3 3 6)))

(test-assert (list-range-check-right-n? '() 10 0 10))
(test-assert (list-range-check-right-n? MediumL 3 97 99))
(test-assert (not (list-range-check-right-n? MediumL 3 0 96)))

(test-equal (take-n BigL 2) '(0 1))
(test-equal (take-n BigL 1) '(0))
(test-assert (null? (take-n BigL 0)))

(test-equal (take-n SmallL 2) '(0))
(test-equal (take-n SmallL 1) '(0))
(test-assert (null? (take-n SmallL 0)))

(test-eq (take-n SmallL 2) SmallL)

(test-equal (take-right-n BigL 2) '(9998 9999))
(test-equal (take-right-n BigL 1) '(9999))
(test-assert (null? (take-right-n BigL 0)))

(test-equal (take-right-n SmallL 2) '(0))
(test-equal (take-right-n SmallL 1) '(0))
(test-assert (null? (take-right-n SmallL 0)))

(test-eq (take-right-n SmallL 2) SmallL)

;; Finish the testsuite, and report results.
(test-end "t-mlg-lists")
