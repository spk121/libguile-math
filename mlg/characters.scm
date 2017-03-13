(define-module (mlg characters)
  #:export (isalnum?
	    isalpha?
	    iscntrl?
	    isdigit?
	    isgraph?
	    islower?
	    isprint?
	    ispunct?
	    isspace?
	    isupper?
	    isxdigit?
	    ascii-isalnum?
	    ascii-isalpha?
	    ascii-iscntrl?
	    ascii-isdigit?
	    ascii-isgraph?
	    ascii-islower?
	    ascii-isprint?
	    ascii-ispunct?
	    ascii-isspace?
	    ascii-isupper?
	    ascii-isxdigit?
	    tolower
	    toupper
	    ))

(define (isalnum? c)
  "Return #t if character C is alphabetic or numeric."
  (char-set-contains? char-set:letter+digit c))

(define (isalpha? c)
  "Return #t if character C is a letter."
  (char-set-contains? char-set:letter c))

(define (iscntrl? c)
  "Return true if C an ISO C0 or C1 control character"
  (char-set-contains? char-set:iso-control c))

(define (isdigit? c)
  "Return #t if C is a digit."
  (char-set-contains? char-set:digit? c))  

(define (isgraph? c)
  "Return #t if C is a graphic character."
  (char-set-contains? char-set:graphic c))

(define (islower? c)
  "Return #t if C a lowercase letter."
  (char-set-contains? char-set:lower-case c))

(define (isprint? c)
  "Return #t if C is a printing character."
   (char-set-contains? char-set:printing c))

(define (ispunct? c)
  "Return #t if C is a graphical character that is not alphanumeric.
Note that this includes both punctuation and symbols."
  (and (char-set-contains? char-set:graphic c)
       (not (char-set-contains? char-set:letter+digit c))))

(define (isspace? c)
  "Return #t if C is a whitespace character."
   (char-set-contains? char-set:whitespace c)

(define (isupper? c)
  "Return #t if C is an uppercase letter."
   (char-set-contains? char-set:uppercase c))

(define (isxdigit? c)
  "Return #t if C is hexadecimal digit."
  (char-set-contains? char-set:hex-digit c))

(define (ascii-isalnum? c)
  "Return true if C is both ASCII and a letter or digit."
  (and
   (char-set-contains? char-set:letter+digit c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isalpha? c)
  "Return true if C is both ASCII and a letter."
  (and
   (char-set-contains? char-set:letter c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-iscntrl? c)
  "Return true if C is both ASCII and a control character"
  (and
   (char-set-contains? char-set:iso-control c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isdigit? c)
  "Return #t if C is both ASCII and a digit."
  (and
   (char-set-contains? char-set:digit? c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isgraph? c)
  "Return #t if C is both ASCII and a graphic character."
  (and
   (char-set-contains? char-set:graphic c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-islower? c)
  "Return #t if C is both ASCII and a lowercase letter."
  (and
   (char-set-contains? char-set:lower-case c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isprint? c)
  "Return #t if C is both ASCII and a printing character."
  (and
   (char-set-contains? char-set:printing c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-ispunct? c)
  "Return #t if C is an ASCII graphical character that is not alphanumeric.
Note that this includes both punctuation and symbols."
  (and (char-set-contains? char-set:graphic c)
       (not (char-set-contains? char-set:letter+digit c))
       (char-set-contains? char-set:ascii c)))

(define (ascii-isspace? c)
  "Return #t if C is both ASCII and a whitespace character."
  (and
   (char-set-contains? char-set:whitespace c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isupper? c)
  "Return #t if C is both ASCII and an uppercase letter."
  (and
   (char-set-contains? char-set:uppercase c)
   (char-set-contains? char-set:ascii c)))

(define (ascii-isxdigit? c)
  "Return #t if C is both ASCII and a hexadecimal digit."
  (and
   (char-set-contains? char-set:hex-digit c)
   (char-set-contains? char-set:ascii c)))

(define (tolower c)
     "Return the lowercase character version of C"
     (char-downcase c))

(define (toupper c)
  "Return the uppercase character version of C"
  (char-upcase c))
