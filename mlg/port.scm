;; Extensions to Guile ports.
(define-module (mlg port)
  #:export (peek-char=?
	    peek-char-ci=?
	    peek-char-isspace?
	    peek-char-isdigit?
	    read-whitespace
	    read-integer
	    read-regex-string
	    ))

;; If p is a C string pointer.
;; *c is peek-char
;; c++ is read-char
;; *c++ is read-char

;; read-char gets 1 char, increments, may return eof

;; peek-char gets 1 char, doesn't increment, may return eof


(define (peek-char-safe port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#\null
	c)))

(define (peek-char=? port c2)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#f
	;; else
	(char=? c c2))))

(define (peek-char-ci=? port c2)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#f
	;; else
	(char-ci=? c c2))))

(define (peek-char-isspace? port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#f
	;; else
	(char-set-contains? char-set:whitespace c))))

(define (peek-char-isdigit? port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#f
	;; else
	(char-set-contains? char-set:digit c))))

(define (peek-2nd-char-safe port)
  (let ((c (peek-char port)))
    (if (eof-object? c)
	#\null
	;; else
	(begin
	  (set! c (read-char port))
	  (let ((c2 (peek-char port)))
	    (unread-char c port)
	    (if (eof-object? c2)
		#\null
		c2))))))

(define (read-char-safe port)
  (let ((c (read-char port)))
    (if (eof-object? c)
	#\null
	c)))

(define* (read-whitespace #:optional (port (current-input-port)))
  "Reads and returns any awating whitespace characters from port.
If whitespace is found, returns it as a string.  If no whitespace is
found, returns an empty string.  If the port is at EOF, returns EOF."
  (let loop ((output ""))
    (let ((c (peek-char port)))
      (if (eof-object? c)
	  ;; We've reached the end
	  (if (string=? output "")
	      ;; We didn't find any whitespace, so return EOF
	      c
	      ;; Otherwise, we can return what we fond
	      output)
	  ;; There is a character to be read.
	  (if (char-set-contains? char-set:whitespace c)
	      ;; Found whitespace, keep going!.
	      (loop (string-append output (string (read-char port))))
	      ;; else, the next char is not whitespace, so we're done.
	      output)))))

(define* (read-integer #:optional (port (current-input-port)) (base 0))
  "Read a string representation of an integer from PORT, assuming
BASE. BASE is either 0, or 2 to 36, with a default value of 0.

If BASE is 0, the number will assumed to be base-16 if it starts with
0x or 0X, base 8 if it starts with 0, or base 10 otherwise.

Will return the integer, or 0 if no integer was found, or EOF"
  (let ((ws (read-whitespace port)))
    (if (eof-object? ws)
	;; Port is EOF; quit now
	ws

	(let ((neg #f)
	      (txt (string)))
	  ;; Look for a Guile style initializer of #x, #o #d or
	  ;; #b, maybe followed by plus or minus.
	  ;; FIXME
	  ;;(if (char=? (peek-char-safe port) #\#)
	  ;;    'fixme)
	  ;; Otherwise,
	  ;; Look for a C style initializer of plus or minus maybe
	  ;; followed by 0x for hex.
	  (if (char=? (peek-char-safe port) #\-)
	      (begin
		(set! neg #t)
		(set! txt (string-append txt (string (read-char port)))))
	      (if (char=? (peek-char-safe port) #\+)
		  (set! txt (string-append txt (string (read-char port))))))
	  
	  (if (and (= base 0)
		   (char=? (peek-char-safe port) #\0)
		   (char-ci=? (peek-2nd-char-safe port) #\x))
	      (begin
		(set! txt (string-append txt (string (read-char port) (read-char port))))
		(set! base 16)))
	  (if (and (= 0 base)
		   (char=? (peek-char-safe port) #\0))
	      (set! base 8))
	  (if (= base 0)
	      (set! base 10))

	  ;; If there isn't at least one good digit, we push
	  ;; back the introducer string and quit.
	  (let ((c (peek-char-safe port)))
	    (let ((val (cond
			((and (char>=? c #\0) (char<=? c #\9))
			 (- (char->integer c) (char->integer #\0)))
			((and (char>=? c #\A)  (char<=? c #\Z))
			 (+ 10 (- (char->integer c) (char->integer #\A))))
			((and (char>=? c #\a)  (char<=? c #\z))
			 (+ 10 (- (char->integer c) (char->integer #\a))))
			(else
			 #f))))
	      (if (not val)
		  (begin
		    (unread-string txt port)
		    0)

		  ;; Otherwise, we loop, gathering digits.
		  (let loop ((c c)
			     (acc 0))
		    (let ((val (cond
				((and (char>=? c #\0) (char<=? c #\9))
				 (- (char->integer c) (char->integer #\0)))
				((and (char>=? c #\A)  (char<=? c #\Z))
				 (+ 10 (- (char->integer c) (char->integer #\A))))
				((and (char>=? c #\a)  (char<=? c #\z))
				 (+ 10 (- (char->integer c) (char->integer #\a))))
				(else
				 #f))))
		      (if (or (not val) (>= val base))
			  ;; return
			  (if neg (- acc) acc)
			  ;; else continue
			  (begin
			    (set! txt (string-append txt (string (read-char port))))
			    (loop (peek-char-safe port)
				  (+ (* acc base) val)))))))))))))

(define *read-regex-string-err* "")

(define (get-read-regex-string-err)
  "If the last call to read-regex-string returned #f, this will give a
text that describes the reason for the failure."
  *read-regex-string-err*)

(define* (read-regex-string #:optional (port (current-input-port)))
  "Extracts the text of a delimited regular expression.  The 1st
character (which should usually be '/') will be the delimiter.  It
searches forward for the matching delimiter, handling shell escapes.
The text of the delimited regular expression is returned.

Return #f if no delimited regular expression was found, or #<eof>
if the port is EOF.

If #f is returned, calling get-read-regex-string-err will return the reason
why no regex string was found."
  
  (set! *read-regex-string-err* "")
  (let ((delimiter (read-char port)))
    
    (cond
     ((eof-object? delimiter)
      (set! *read-regex-string-err* "eof")
      delimiter)

     ((eof-object? (peek-char port))
      ;; Only 1 char available.  Not a regex.
      (set! *read-regex-string-err* "only one char available")
      (unread-char delimiter)
      #f)
     
     ((char=? (peek-char port) delimiter)
      ;; Two delimiters in a row.  An empty regex.
      (read-char port)
      (string delimiter delimiter))

     (else
      (read-regex-pattern-string port delimiter)))))

(define (read-regex-pattern-string port delimiter)
  "Extracts a regex pattern string terminated by delimiter, handline
escapes and looking for errors.  Returns the pattern string, or
#f if no valid pattern string was found."
  (let loop ((c (peek-char port))
	     (txt (string delimiter)))
    (cond
     ((char=? c #\[)
      (let ((cclass (read-char-set-string port)))
	(if (not cclass)
	    (begin
	      (unread-string txt port)
	      #f)
	    (loop (peek-char-safe port)
		  (string-append txt cclass)))))

     ((char=? c #\\)
      (if (char=? #\null (peek-2nd-char-safe port))
	  (begin
	    (set! *read-regex-string-err* "trailing backslash")
	    (unread-string txt port)
	    #f)
	  ;; Else this is a character escape.
	  (begin
	    (read-char port)
	    (let ((c2 (read-char port)))
	      (loop (peek-char-safe port)
		    (string-append txt (string c c2)))))))

     ((char=? c #\null)
      ;; Never found matching delimiter
      (set! *read-regex-string-err* "no matching delimiter found")
      (unread-string txt port)
      #f)

     ((char=? c delimiter)
      ;; Success
      (read-char port)
      (string-append txt (string c)))

     (else
      ;; Keep looking.
      (read-char port)
      (loop (peek-char-safe port)
	    (string-append txt (string c)))))))

(define (read-char-set-string port)
  "Reads a POSIX regex character set string from PORT, returning 
the string, or #f, if not valid character class string was found."
  (let ((opener (read-char port))
	(txt ""))
    (set! txt (string-append txt (string opener)))
    (let ((c (peek-char-safe port)))

      ;; '^' and ']' at the beginning of character classes need to
      ;; be treated differently.
      (when (char=? c #\^)
	(read-char port)
	(set! txt (string-append txt (string c)))
	(set! c (peek-char-safe port)))

      (when (char=? c #\])
	(read-char port)
	(set! txt (string-append txt (string c)))
	(set! c (peek-char-safe port)))

      ;; Now we just look for the closing bracket, but, if we find
      ;; another opening bracket for a predefined character class of
      ;; the form [:alpha:], we have to handle those nested
      ;; brackets, too.
      (let loop ((c c))
	(let ((d (peek-2nd-char-safe port)))
	  (cond
	   ((and (char=? c #\[)
		 (member d (string->list ".:=")))
	    (begin
	      (read-char port)
	      (read-char port)
	      (set! txt (string-append txt (string c d)))
	      (let loop2 ((d2 (peek-char-safe port))
			  (c2 (peek-2nd-char-safe port)))
		;; (pk 'loop2 'c c 'd d 'd2 d2 'c2 c2)
		(cond
		 ((or (char=? d2 #\null)
		      (char=? c2 #\null))
		  (set! *read-regex-string-err*
		    (format #f "premature termination of character class '~a'" txt))
		  (unread-string txt port)
		  #f)
		   
		 ((and (not (char=? d2 d)) (char=? c2 #\]))
		  (set! *read-regex-string-err*
		    (format #f "mismatched character class delimiters ~a != ~a"
			    d d2))
		  (unread-string txt port)
		  #f)
		   
		 ((and (char=? d2 d) (char=? c2 #\]))
		  ;; OK: matching predefined character class delimiters
		  (read-char port)
		  (read-char port)
		  (set! txt (string-append txt (string d2 c2)))
		  txt)
		   
		 (else
		  ;; Keep looping
		  (set! txt (string-append txt (string (read-char port))))
		  (loop2 (peek-char-safe port) (peek-2nd-char-safe port)))))))

	     ((char=? c #\null)
	      (set! *read-regex-string-err* "premature termination or character set")
	      (unread-string txt port)
	      #f)
	     
	     ((char=? c #\])
	      ;; Successful completion.
	      (read-char port)
	      (set! txt (string-append txt (string c)))
	      txt)

	     (else
	      (read-char port)
	      (set! txt (string-append txt (string c)))
	      (loop (peek-char-safe port)))))))))   
