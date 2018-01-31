(define-module (ryu printf)
  #:export (format-signed-decimal
	    format-unsigned))
	    
;; a positional argument 0-9 and $
;; flags like apostrophe, minus, plus, space, pound sign, zero, I
;; A field width that starts with asterisk
;; A precision that starts with period
;; Size specifiers
;; Types


;; Flags: 'signed 'space-signed 'left-justified 'alternative 'zero-padded
;; Length: 'char 'short (normal) 'long 'longlong 'intmax 'size_t 'ptrdiff_t 'longdouble
;; : 'signed 'unsigned 'octal 'hex 'double 'float-decimal 'float-scientific 'float-variable
;; 'float-hex 'char 'string 'pointer

;; FLAGS
;; FIELD WIDTH
;; PRECISION
;; LENGTH MODIFIER
;; CONVERSION SPECIFIER

(define (format-signed-decimal val flags field-width precision)
  "Prints VAL as a base-10, signed integer.
FLAGS is a list with zero or more of 'signed 'space-signed 'left-justified
 'zero-padded
FIELD_WIDTH is the minimum output string length.
PRECISION is the minimum number of digits displayed; leading
 zeros are added if necessary.
Cannot be both 'signed and 'space-signed. 'signed has precedence.
Cannot have a PRECISION > 0 and be 'zero-padded. PRECISION has precedence."
  (let* ((numstr (if (and (zero? val) (zero? precision))
		     ""
		     ;; else
		     (number->string (abs val))))
	 (signstr
	  (cond
	    ((< val 0) "-")
	    ((member 'signed flags) "+")
	    ((member 'space-signed flags) " ")
	    (else "")))
	 (num-len (string-length numstr))
	 (num-sign-len (+ num-len
			  (string-length signstr)))
	 (precision-pad-len (max 0 (- precision num-len)))
	 (field-pad-len (max 0 (- field-width (+ num-sign-len
						 precision-pad-len)))))
    (if (and (zero? val) (zero? precision))
	;; There is a special case when val and precision are both
	;; zero where no digits are printed.
	(make-string field-width #\space)
	;; else, at least one digit is printed.
	(string-append
	 ;; left pad
	 (if (and (member 'zero-padded flags)
		  (not (member 'left-justified flags))
		  (= precision 0))
	     ;; left pad is zeros
	     (make-string field-pad-len #\0)
	     ;; else
	     (if (not (member 'left-justified flags))
		 ;; left pad is spaces
		 (make-string field-pad-len #\space)
		 ;; else
		 ""))
	 ;; sign
	 signstr
	 ;; precision padding
	 (make-string precision-pad-len #\0)
	 ;; the value
	 numstr
	 ;; right padding
	 ( if (member 'left-justified flags)
	      (make-string field-pad-len #\space)
	      "")))))

(define (format-unsigned val flags field-width precision specifier)
  "Prints VAL as an unsigned integer.
FLAGS is a list with zero or more of 'left-justified 'alternate 'zero-padded
FIELD_WIDTH is the minimum output string length.
PRECISION is the minimum number of digits displayed; leading
 zeros are added if necessary.
SPECIFIER is one of 'octal 'unsigned 'lowercase-hex or 'uppercase-hex
Cannot have a PRECISION > 0 and be 'zero-padded. PRECISION has precedence.
When 'octal, 'alternate increases the precision, if necessary, to ensure
  a leading zero.
When 'lowercase-hex or 'uppercase-hex and VAL != 0, a '0x' or '0X' prefix is added."
  (let* (
	 ;; First, the base number
	 (numstr1 (if (and (zero? val) (zero? precision))
		      ""
		      ;; else
		      (number->string (abs val)
				      (cond
				       ((eqv? specifier 'octal) 8)
				       ((or (eqv? specifier 'lowercase-hex)
					    (eqv? specifier 'uppercase-hex))
					16)
				       (else 10)))))
	 ;; Then, the precision
	 (numstr2
	  (string-append (make-string
			  (max 0 (- precision (string-length numstr1)))
			  #\0)
			 numstr1))
	 ;; Then, any alternate text
	 (numstr3
	  (string-append
	   (cond
	    ((and (eqv? specifier 'octal)
		  (member 'alternate flags)
		  (not (zero? (string-length numstr2)))
		  (not (char=? #\0 (string-ref numstr2 0))))
	     "0")
	    ((and (or (eqv? specifier 'uppercase-hex)
		      (eqv? specifier 'lowercase-hex))
		  (and (not (zero? val))
		       (member 'alternate flags)))
	     "0x")
	    (else
	     ""))
	   numstr2))
	 ;; The proper casing
	 (numstr
	  (if (eqv? specifier 'uppercase-hex)
	      (string-upcase numstr3)
	      numstr3))
	 (num-len (string-length numstr))
	 (precision-pad-len (max 0 (- precision num-len)))
	 (field-pad-len (max 0 (- field-width (+ num-len
						 precision-pad-len)))))
    (if (and (zero? val) (zero? precision))
	;; There is a special case when val and precision are both
	;; zero where no digits are printed.
	(make-string field-width #\space)
	;; else, at least one digit is printed.
	(string-append
	 ;; left pad
	 (if (and (member 'zero-padded flags)
		  (not (member 'left-justified flags))
		  (= precision 0))
	     ;; left pad is zeros
	     (make-string field-pad-len #\0)
	     ;; else
	     (if (not (member 'left-justified flags))
		 ;; left pad is spaces
		 (make-string field-pad-len #\space)
		 ;; else
		 ""))
	 ;; precision padding
	 (make-string precision-pad-len #\0)
	 ;; the value
	 numstr
	 ;; right padding
	 ( if (member 'left-justified flags)
	      (make-string field-pad-len #\space)
	      "")))))
