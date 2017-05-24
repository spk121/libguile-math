(define-module (mlg ed address)
  #:use-module (srfi srfi-1)
  #:use-module (oop goops)
  #:use-module (oop goops describe)
  #:use-module (mlg port)
  #:use-module (mlg characters)
  #:use-module (mlg logging)
  #:use-module (mlg typechecking)
  #:export (addr-get-range
	    addr-get-range-error))

(define *compute-ed-address-error* "")

(define (addr-get-range-error)
  *compute-ed-address-error*)

(define (addr-get-range port
			current-line
			last-line
			bmark-callback
			regex-callback)
  (warn-if-false (input-port? port))
  (warn-if-false (integer-nonnegative? current-line))
  (warn-if-false (integer-nonnegative? last-line))
  
  (set! *compute-ed-address-error* "")
  (let ((ret (compute-ed-address (parse-ed-address port)
				 current-line
				 last-line
				 bmark-callback
				 regex-callback)))
    (if ret
	(take-up-to-two-right ret)
	#f)))

(define (compute-ed-address _tokens
			    current-line
			    last-line
			    bmark-callback
			    regex-callback)
  "Convert the address token list into a list of line numbers.

bmark-callback is a lambda that takes a single char and returns a
bookmark location line number or #f.

regex-callback is a lambda that takes 3 variables: regex-string, the
staring line number, and +1 or -1 for direction, and returns a line
number or #f.  If regex-string is an empty string, the last regex
used should be reapplied."
  (cond
   ;; Handle some very common special forms first.
   
   ;; "x" => (null-address 0 null-separator)
   ((equal? _tokens '(null-address 0 null-separator))
    '())

   ;; ",x" => (null-address 0 comma null-address 0 null-separator)
   ((equal? _tokens '(null-address 0 comma null-address 0 null-separator))
    `(1 ,last-line))

   ;; ";x" => (null-address 0 comma null-address 0 null-separator)
   ((equal? _tokens '(null-address 0 semicolon null-address 0 null-separator))
    `(,current-line ,last-line))

   (else
    ;; The comma alone or semicolon alone operations need to be handled
    ;; differently.
    (let ((comma-alone (and (equal? (list-ref _tokens 0) 'null-address)
			    (equal? (list-ref _tokens 2) 'comma)
			    (equal? (list-ref _tokens 3) 'null-address)))
	  (semicolon-alone (and (equal? (list-ref _tokens 0) 'null-address)
				(equal? (list-ref _tokens 2) 'semicolon)
				(equal? (list-ref _tokens 3) 'null-address))))
      
      ;; We loop over all the addresses, building up a list.
      (let loop ((iter 0)
		 (tokens _tokens)
		 (cur-addr current-line)
		 (prev-addr current-line)
		 (addr-list '()))
	(let* ((op (list-ref tokens 0))
	       (offset (list-ref tokens 1))
	       (separator (list-ref tokens 2))
	       (rest (drop tokens 3))
	       (new-addr
		(cond
		 ;; Handle the first iteration special forms
		 ((= iter 0)
		  (cond
		   (comma-alone       1)
		   (semicolon-alone   cur-addr)
		   ((and (eqv? op 'null-address) (eqv? separator 'comma))
		    1)
		   ((and (eqv? op 'null-address) (eqv? separator 'semicolon))
		    cur-addr)
		   ((and (pair? op) (eqv? (car op) 'absolute-number))
		    (set! cur-addr (+ (cdr op) offset))
		    cur-addr)
		   (else
		    ;; Handle the first iteration's regular forms
		    (find-new-addr iter op offset prev-addr cur-addr last-line bmark-callback regex-callback))))

		  ((= iter 1)
		   ;; Handle the second iteration's special forms
		   (cond
		    ((and (= iter 1) (or comma-alone semicolon-alone))
		     last-line)
		    (else
		     ;; Handle the second iteration's regular forms
		     (find-new-addr iter op offset prev-addr cur-addr last-line bmark-callback regex-callback))))

		  (else
		   ;; Handle the later iterations
		   (find-new-addr iter op offset prev-addr cur-addr last-line bmark-callback regex-callback)))))
	  (if (not (null? rest))
	      (loop (1+ iter)
		    rest
		    ;; When the separator is a semicolon, the current
		    ;; address is updated.  When it is a comma, it is
		    ;; not.
		    (if (eqv? separator 'semicolon)
			new-addr
			cur-addr)
		    ;; But the prev-addr is updated always in case we
		    ;; later have a null address.
		    new-addr
		    (append addr-list (list new-addr)))
	      ;; else, we're done
	      (let ((final-addr-list (append addr-list (list new-addr))))
		(validate-ed-address final-addr-list last-line)))))))))

(define (find-new-addr iter op offset prev-addr cur-addr last-line bmark-callback regex-callback)
  (cond
   ;; But, when later arguments are missing, they are
   ;; just set to the last address.
   ((and (> iter 0) (eqv? op 'null-address))
    prev-addr)

   ((eqv? op 'period)
    cur-addr)

   ((eqv? op 'dollar)
    last-line)

   ((member op '(slash double-slash))
    (if (not cur-addr)
	#f
	(let ((rx-line (regex-callback "" cur-addr +1)))
	  (unless rx-line
	    (set! *compute-ed-address-error*
	      "No forward regex match found for previous regex"))
	  (add-num-or-false rx-line offset))))
		  
   ((member op '(question-mark double-question-mark))
    (if (not cur-addr)
	#f
	(let ((rx-line (regex-callback "" cur-addr -1)))
	  (unless rx-line
	    (set! *compute-ed-address-error*
	      "No backward regex match found for previous regex"))
	  (add-num-or-false rx-line offset))))

   ((eqv? (car op) 'absolute-number)
    (when (= iter 0)
      (set! cur-addr (+ (cdr op) offset)))
    (+ (cdr op) offset))
		  
   ((eqv? (car op) 'relative-number)
    (add-num-or-false cur-addr (cdr op) offset))
   
   ((eqv? (car op) 'apostrophe)
    (let ((bmark-line (bmark-callback (cdr op))))
      (unless bmark-line
	(set! *compute-ed-address-error*
	  (format #f "Unknown bookmark '~a'" (cdr op))))
      (add-num-or-false bmark-line offset)))

   ((eqv? (car op) 'forward-regex 'backward-regex)
    (cond
     ((not cur-addr)
      #f)
     ((not (cdr op))
      (set! *compute-ed-address-error*
	(get-read-regex-string-err))
      #f)
     (else
      ;; Strip the delimiters '/' or '?' and run the regex.
      (let ((rx-line (regex-callback (substring (cdr op) 1 (1- (string-length (cdr op))))
				     cur-addr
				     (if (eqv? (car op) 'forward-regex)
					 +1
					 -1))))
	(unless rx-line
	  (set! *compute-ed-address-error*
	    (format #f "No regex match found for '~a'"
		    (cdr op))))
	(add-num-or-false rx-line offset)))))))

(define* (parse-ed-address #:optional (port (current-input-port)))
  (let loop ((output '()))
    (let* ((addr (read-ed-address port))
	   (offset (if (eqv? addr 'null-address)
		       0
		       (read-ed-offset port)))
	   (separator (read-ed-separator port)))
      ;;(pk addr offset separator)
      (if (eqv? separator 'null-separator)
	  (append output (list addr offset separator))
	  ;; else
	  (loop (append output (list addr offset separator))))
      )))

(define* (add-num-or-false a b #:optional (c 0))
  (if (or (not (integer? a)) (not (integer? b)) (not (integer? c)))
      #f
      (+ a b c)))

(define (take-up-to-two-right lst)
  (warn-if-false (list? lst))
  
  (if (>= (length lst) 2)
      (take-right lst 2)
      lst))

(define (last-two-list-elements-in-range? lst low high)
  "Return #t if the last two elements in the list, if present, are
between low (inclusive) and high (inclusive)"
  (warn-if-false (list? lst))
  (warn-if-false (integer-nonnegative? low))
  (warn-if-false (integer-nonnegative? high))
  
  (let ((sublist (take-up-to-two-right lst)))
    (every (lambda (x)
	     (and (integer? x) (<= low x) (<= x high)))
	   sublist)))

(define (validate-ed-address final-addr-list last-line)
  ;; The last two elements are the one that might be used.  Any other
  ;; initial elements can be ignored.  The last two elements must be
  ;; between 0 and last-line.
  (if (last-two-list-elements-in-range? final-addr-list 0 last-line)
      final-addr-list
      (begin
	(set! *compute-ed-address-error*
	  (format #f "Address ~s out of range ~a to ~a"
		  final-addr-list 0 last-line))
	#f)))


(define* (read-ed-address #:optional (port (current-input-port)))
  ;; N.B.: I can't tell from the standard if I shall gobble
  ;; whitespace here.
  (read-whitespace port)
  
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c)
      'null-address)

     ;; The period character shall address the current line.
     ((char=? c #\.)
      (read-char port)
      'period)

     ;; The dollar sign character shall address the last line of the buffer.
     ((char=? c #\$)
      (read-char port)
      'dollar)
     
     ;; A positive decimal number shall address the nth line of the buffer.
     ((member c (string->list "0123456789"))
      (cons 'absolute-number (read-integer port)))

     ;; An apostrophe followed by a single lowercase letter is a bookmark
     ((and (char=? c #\') (islower? (peek-2nd-char-safe port)))
      (read-char port)
      (cons 'apostrophe (read-char port)))
     
     ;; A slash at the end of the line indicates repeating the last regex
     ((and (char=? c #\/) (char=? #\null (peek-2nd-char-safe port)))
      (read-char port)
      'single-slash)

     ;; A double slash indicates repeating the last regex
     ((and (char=? c #\/) (char=? #\/ (peek-2nd-char-safe port)))
      (read-char port)
      (read-char port)
      'double-slash)

     ((char=? c #\/)
      (cons 'forward-regex (read-regex-string port)))
     
     ;; A question mark the end of the line indicates repeating the last regex
     ((and (char=? c #\?) (char=? #\null (peek-2nd-char-safe port)))
      (read-char port)
      'single-question-mark)

     ;; A double question mark is repeating the last regex
     ((and (char=? c #\?) (char=? #\? (peek-2nd-char-safe port)))
      (read-char port)
      (read-char port)
      'double-question-mark)

     ((char=? c #\?)
      (cons 'backward-regex (read-regex-string port)))
     
     ;; A plus sign not followed by a number is the current line plus 1
     ((and (char=? c #\+) (not (isdigit? (peek-2nd-char-safe port))))
      (read-char port)
      (cons 'relative-number 1))

     ;; A minus sign not followed by a number is the current line plus 1
     ((and (char=? c #\-) (not (isdigit? (peek-2nd-char-safe port))))
      (read-char port)
      (cons 'relative-number -1))

     ;; A plus sign followed by a number is current line + num
     ((and (char=? c #\+) (isdigit? (peek-2nd-char-safe port)))
      (read-char port)
      (cons 'relative-number (read-integer port)))

     ;; A minus sign followed by a number is current line - num
     ((and (char=? c #\-) (isdigit? (peek-2nd-char-safe port)))
      (read-char port)
      (cons 'relative-number (- (read-integer port))))

     ;; else there is no address. Jump to separator search.
     (else
      'null-address))))

(define* (read-ed-offset #:optional (port (current-input-port)))
  (let loop ((delta 0))
    (read-whitespace port)
    (let ((c (peek-char port)))
      (cond
       ((eof-object? c)
	delta)

       ;; A positive decimal number shall be a positive offset
       ((member c (string->list "0123456789"))
	(loop (+ delta (read-integer port))))
       
       ;; A plus sign not followed by a number is an offset of 1
       ((and (char=? c #\+) (not (isdigit? (peek-2nd-char-safe port))))
	(read-char port)
	(loop (1+ delta)))
       
       ;; A minus sign not followed by a number is an offset of 1
       ((and (char=? c #\-) (not (isdigit? (peek-2nd-char-safe port))))
	(read-char port)
	(loop (1- delta)))

       ;; A plus sign followed by a positive offset
       ((and (char=? c #\+) (isdigit? (peek-2nd-char-safe port)))
	(read-char port)
	(loop (+ (read-integer port))))

       ;; A minus sign followed by a number is a negative offset
       ((and (char=? c #\-) (isdigit? (peek-2nd-char-safe port)))
	(read-char port)
	(loop (- delta (read-integer))))

       ;; else there are no more offsets.
       (else
	delta)))))

(define* (read-ed-separator #:optional (port (current-input-port)))
  (let ((c (peek-char port)))
    (cond
     ((eof-object? c)
      'null-separator)

     ((char=? c #\,)
      (read-char port)
      'comma)

     ((char=? c #\;)
      (read-char port)
      'semicolon)

     (else
      'null-separator))))
