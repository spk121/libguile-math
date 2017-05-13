(define-module (mlg strings)
  #:use-module (mlg assert)
  #:export (string-ends-with?
	    string-starts-with?
	    string-strip-escapes
	    string->ed-escaped-string))

(define (string-ends-with? str char)
  "Return #t is the last character in string STR
is CHAR."
  (assert (string? str))
  (assert (char? char))
  (if (string-null? str)
      #f
      (char=? char (string-ref str (1- (string-length str))))))

(define (string-starts-with? str char)
  "Return #t is the first character in string STR
is CHAR."
  (assert (string? str))
  (assert (char? char))
  (if (string-null? str)
      #f
      (char=? char (string-ref str 0))))

(define (string-strip-escapes str)
  "Returns a copy of STR with backslash string escapes removed."
  (let* ((esc #f)
	 (newstr (string-fold
		  (lambda (c prev)
		    (cond
		     ((and (char=? c #\\) (not esc))
		      (set! esc #t)
		      prev)
		     (else
		      (set! esc #f)
		      (string-append prev (string c)))))
		  ""
		  str)))
    (if esc
	(string-append newstr (string #\\))
	newstr)))

(define (string->ed-escaped-string str)
  "Converts a string into another string. Non-printable characters
will be converted into the escape sequences that POSIX 'ed' prefers.
Dollar signs will be escaped, as well."
  (string-fold
   (lambda (c prev)
     (cond
      ((char=? c #\\)
       (string-append prev (string #\\ #\\)))
      ((char=? c #\alarm)
       (string-append prev (string #\\ #\a)))
      ((char=? c #\backspace)
       (string-append prev (string #\\ #\b)))
      ((char=? c #\page)
       (string-append prev (string #\\ #\f)))
      ((char=? c #\return)
       (string-append prev (string #\\ #\r)))
      ((char=? c #\tab)
       (string-append prev (string #\\ #\t)))
      ((char=? c #\vtab)
       (string-append prev (string #\\ #\v)))
      ((char=? c #\$)
       (string-append prev (string #\\ #\$)))
      ((not (char-set-contains? char-set:printing c))
       (string-append prev (format #f "\\~3,'0o" (char->integer c))))
      (else
       (string-append prev (string c)))))
   ""
   (string-normalize-nfc str)))
