(define-module (mlg spreadsheet)
  #:use-module (ice-9 i18n)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (rnrs bytevectors)
  #:export (clean
	    trim))

;; An xl-error is basically an enumerated type indicating
;; an error.  It is one of
;; #NULL!        1
;; #DIV/0!       2
;; #VALUE!       3
;; #REF!         4
;; #NAME?        5
;; #NUM!         6
;; #N/A          7
;; #GETTING_DATA 8

(define-class <xl-error>
  (type #:init-value #f #:getter get-type #:setter set-type! #init-keyword #:type))

(define-method ())
(define char-set:clean
  (char-set-union (char-set #\space)
		  char-set:graphic))

(define (clean str)
  "Return a new copy of STR with all non-printable characters removed.
Also, all whitespace other than a standard space is removed."
  (let ((str2 (make-string (string-length str)))
	(pos 0))
    (string-for-each
     (lambda (c)
       (when (char-set-contains? char-set:clean c)
	 (string-set! str2 pos c)
	 (set! pos (1+ pos))))
     str)
    (substring str2 0 pos)))

(define (trim str)
  "Given a string, remove whitespace at the beginning and end of the
string. Also replace any instance of one or more whitespace characters
in the string with a single space."
  (let ((str2 (make-string (string-length str)))
	(pos 0)
	(last-char-was-whitespace? #f))
    (string-for-each
     (lambda (c)
       (cond
	((not (char-set-contains? char-set:whitespace c))
	 (string-set! str2 pos c)
	 (set! pos (1+ pos))
	 (set! last-char-was-whitespace? #f))
	((not last-char-was-whitespace?)
	 (string-set! str2 pos #\space)
	 (set! pos (1+ pos))
	 (set! last-char-was-whitespace? #t))
	;; else
	;; Duplicate whitespace
	 ))
     str)
    (string-trim-both str2 (lambda (c)
			     (char-set-contains? char-set:whitespace c))
		      0 pos)))
  
(define (lower str)
  "Return a new string, converting all characters into lower case."
  (string-locale-downcase str))

(define (upper str)
  "Return a new string, converting all characters into upper case."
  (string-locale-upcase str))

(define (proper str)
  "Return a new string, title-casing the string"
  (string-locale-titlecase str))

;; FIXME: get the number of decimals printed correct. Right now
;; it elides decimals if they are zeroes.
(define* (currency num #:optional (decimals 0))
  "Round a number to the given number of decimals then return a string
of that number in the current currency format."
  (let* ((divisor (expt 10 (- decimals)))
	 (rounded-num (* divisor (centered-quotient num divisor))))
    (monetary-amount->locale-string rounded-num #f)))

(define* (fixed num #:optional (decimals 2) (no-commas? #f))
  "Converts a number to a locale string with a certain number
of decimal places."
  (let* ((divisor (expt 10 (- decimals)))
	 (rounded-num (* divisor (centered-quotient num divisor)))
	 ;; Epsilon is a small number added to NUM to make sure that
	 ;; zeroes to the right of the decimal point aren't elided.
	 (epsilon (if (<= decimals 0)
		      0
		      (expt 10.0 (- -1 decimals)))))
  (number->locale-string (+ rounded-num epsilon)  (max 0 decimals))))

;; (define (value txt)
;;   "Converts a locale string into a number"
;;   (locale-string->inexact txt))

(define (char num)
  "Converts NUM, a number between 1 and 255, into a character.
For UTF8 locales, returns a Unicode codepoint between #\x01 and
#xFF. For ISO 8859 locales or other 8-bit locales, returns a character
appropriate for that locale.  The return value for non-UTF, non-8-bit
locales is unspecified."
  (let* ((enc (locale-encoding))
	 (utf? (string-contains-ci enc "utf")))
    (when (and (> num 0) (<= num 255))
      (if utf?
	  (integer->char num)
	  (bytevector->string (make-bytevector 1 num) enc)))))

(define (unichar num)
  "Converts NUM, a number, into a Unicode codepoint"
  (integer->char num))

(define (unicode str)
  "Returns the codepoint of the first character in STR."
  (if (char? str)
      (char->integer str)
      (if (string? str)
	  (char->integer (string-ref str 0)))))

;; AND exists

;; OR exists

(define (xor . args)
  "Return #t if an odd number of arguments evaluates to #t"
  (odd? (count (lambda (x) (not (not x))) args)))

;; NOT exists

(define (true)
  "A function that returns #t"
  #t)

(define (false)
  "A function that returns #f"
  #f)

(define (iserror x)
  "Return #t if the expression X causes an exception."
  (not (false-if-exception x)))

(define (isnumber x)
  "Return #t if x is any kind of number."
  (number? x))

(define (iseven x)
  "Return #t if x is even."
  (even? x))

(define (isodd x)
  "Return #t if x is an odd number."
  (odd? x))

(define (n x)
  "Converts X into a number. Numbers remain the same.
Dates and times are converted into microseconds since 1970.  #t and #f
are converted to 1 and 0 respectively.  Notably, strings, even if they
contain numbers, return 0."
  (cond
   ((eq? x #t)
    1)
   ((eq? x #f)
    0)
   ((number? x)
    x)
   (
    ;; FIXME: need a whole library support to deal with dates
    0)))

;; ABS exists

(define (sign num)
  "Returns the sign of NUM, either +1, -1, or 0"
  (if (real? num)
      (if (< num 0.0)
	  -1
	  (if (> num 0.0)
	      1
	      0))
      (if (< num 0)
	  -1
	  (if (> num 0)
	      1
	      0))))


;; GCD exists

;; LCM exists

(define (sum . lst)
  "Sum the arguments."
  (apply + lst))

(define (product . lst)
  "Compute the product of the arguments."
  (apply * lst))

(define (power n p)
  "Returns a given number raised to the supplied power."
  (expt n p))

;; SQRT exists

;; QUOTIENT exists

(define (mod numerator denominator)
  "Compute the remainder of a division between two supplied numbers."
  (modulo numerator denominator))

;; ceiling exists

(define* (ceiling-precise number #:optional (significance 1))
  "Rounds a number up to the nearest multiple of SIGNIFICANCE.
Note that the sign of SIGNIFICANCE is ignored."
  ;; FIXME
  (if (zero? significance)
      0
      ;; else
      (* (abs significance)
	 (ceiling-quotient number (abs significance)))))

(define* (floor-precise number #:optional (significance 1))
  "Rounds a number down to the nearest multiple of SIGNIFICANCE.
Note that the sign of SIGNIFICANCE is ignored."
  ;; FIXME
  (if (zero? significance)
      0
      ;; else
      (* (abs significance)
	 (floor-quotient number (abs significance)))))

(define (int num)
  "Round down to the nearest integer."
  (inexact->exact (floor num)))


