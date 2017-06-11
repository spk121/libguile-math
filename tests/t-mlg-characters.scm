(use-modules (srfi srfi-64)
	     (mlg characters))

(test-begin "t-mlg-characters")

(test-equal (ascii-isdigit? #\a) #f)
(test-equal (ascii-isdigit? #\0) #t)
;; U+0661 ARABIC-INDIC DIGIT ONE
(test-equal (ascii-isdigit? #\x0661) #f)

(test-equal (U+ #x0661) (integer->char #x0661))
(test-equal (U+ #x0020) #\space)

(test-end "t-mlg-characters")
