(define-module (mlg ed ops)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (mlg logging)
  #:use-module (mlg port)
  #:use-module (mlg procedure)
  #:use-module (mlg strings)
  #:use-module (mlg typechecking)
  #:use-module (gano CBuffer)
  #:export (op-get-dispatch-error
	    op-dispatch))

(define EOF -1)
(define ERR -2)
(define EMOD -3)
(define FATAL -4)

(define *op-dispatch-error* "")

(define (op-get-dispatch-error)
  *op-dispatch-error*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (op-append cbuf addr special suffix append)
  "Appends lines after the given address."
  (warn-if-false (cbuffer? cbuf))
  (warn-if-false (list-length-1? addr))
  (warn-if-false (list-of-integers? addr))
  (warn-if-false (list-of-strings? append))
  (warn-if-false (string? suffix))
  
  (ed-append cbuf (first addr) append)
  
  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (display-lines cbuf (get-line-cur cbuf) (1+ (get-line-cur cbuf)) suffix))
  0)

(define (op-change cbuf addr special suffix append)
  "Appends lines after the given address."
  (warn-if-false (cbuffer? cbuf))
  (warn-if-false (list-length-2? addr))
  (warn-if-false (list-of-integers? addr))
  (warn-if-false (list-of-strings? append))
  (warn-if-false (string? suffix))
  (log-debug-locals)
  
  (ed-change cbuf (max 0 (1- (first addr))) (second addr) append)
  
  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (display-lines cbuf (get-line-cur cbuf) (1+ (get-line-cur cbuf)) suffix))
  0)

(define (put-tty-line str n suffix)
  "Print text to stdout."
  (warn-if-false (string? str))
  (warn-if-false (integer? n))
  (warn-if-false (string? suffix))
  
  (if (member #\n (string->list suffix))
      (format #t "~d~/" n))
  (if (member #\l (string->list suffix))
      (format #t "~a$~%" (string->ed-escaped-string str))
      (format #t "~a~%" str)))

(define (display-lines cbuf from to suffix)
  "Print a range of lines to stdout."
  (warn-if-false (cbuffer? cbuf))
  (warn-if-false (integer-nonnegative? from))
  (warn-if-false (integer-nonnegative? to))
  (warn-if-false (<= from to))
  (warn-if-false (string? suffix))
  
  (cond
   ((or (zero? from) (zero? to))
    (set! *op-dispatch-error* "invalid address")
    (log-debug "address (~a,~a) <= 0" from to)
    ERR)
   (else
    (do ((i from (1+ i)))
	((>= i to))
      (put-tty-line (get-text-line cbuf (1- i)) i suffix))
    0)))


(define op-delete ...)
(define op-edit ...)
(define op-edit-without-checking ...)
(define op-filename ...)
(define op-global ...)
(define op-global-interactive ...)
(define op-help ...)
(define op-help-mode ...)
(define op-insert ...)
(define op-join ...)
(define op-mark ...)
(define op-list ...)
(define op-move ...)
(define op-number ...)
(define op-print ...)
(define op-prompt ...)
(define op-quit ...)
(define op-quit-without-checking ...)
(define op-read ...)
(define op-substitute ...)
(define op-copy ...)
(define op-undo ...)
(define op-global-non-matched ...)
(define op-global-interactive-non-matched ...)
(define op-write ...)
(define op-line-number ...)
(define op-shell-escape ...)
(define op-null ...)

(define dispatch-table
  ;; 1. Shortcut character
  ;; 2. Required number of addresses
  ;; 3,4. The default addresses
  ;; 5. How to parse additional info
  ;; 6. Does command accept standard suffix
  ;; 7. Does command accept input lines
  ;; 8. Operation function
  `((#\a    1 dot   #f    null          #t #t  ,op-append)
    (#\c    2 dot   dot   null          #t #t  ,op-change)
    (#\d    2 dot   dot   null          #t #f  ,op-delete)
    (#\e    0 #f    #f    file          #f #f  ,op-edit)
    (#\E    0 #f    #f    file          #f #f  ,op-edit-without-checking)
    (#\f    0 #f    #f    file          #f #f  ,op-filename)
    (#\g    2 1     $     regex+cmd     #t #f  ,op-global)
    (#\G    2 1     $     regex         #t #f  ,op-global-interactive)
    (#\h    0 #f    #f    null          #t #f  ,op-help)
    (#\H    0 #f    #f    null          #t #f  ,op-help-mode)
    (#\i    1 dot   #f    null          #t #t  ,op-insert)
    (#\j    2 dot   dot+1 null          #t #f  ,op-join)
    (#\k    1 dot   #f    bmark         #t #f  ,op-mark)
    (#\l    2 dot   dot   null          #t #f  ,op-list)
    (#\m    2 dot   dot   address       #t #f  ,op-move)
    (#\n    2 dot   dot   null          #t #f  ,op-number)
    (#\p    2 dot   dot   null          #t #f  ,op-print)
    (#\P    0 #f    #f    null          #t #f  ,op-prompt)
    (#\q    0 #f    #f    null          #f #f  ,op-quit)
    (#\Q    0 #f    #f    null          #f #f  ,op-quit-without-checking)
    (#\r    1 $     #f    file          #f #f  ,op-read)
    (#\s    2 dot   dot   regex+replace #t #f  ,op-substitute)
    (#\t    2 dot   dot   address       #t #f  ,op-copy)
    (#\u    0 #f    #f    null          #t #f  ,op-undo)
    (#\v    2 1     $     regex+cmd     #t #f  ,op-global-non-matched)
    (#\V    2 1     $     regex         #t #f  ,op-global-interactive-non-matched)
    (#\w    2 1     $     file          #f #f  ,op-write)
    (#\=    1 $     #f    null          #t #f  ,op-line-number)
    (#\!    0 #f    #f    shell         #f #f  ,op-shell-escape)
    (#\nul  1 dot+1 #f    null          #t #f  ,op-null)))

(define (dispatch:key x)        (list-ref x 0))
(define (dispatch:addr-count x) (list-ref x 1))
(define (dispatch:addr-start x) (list-ref x 2))
(define (dispatch:addr-end x)   (list-ref x 3))
(define (dispatch:parser x)     (list-ref x 4))
(define (dispatch:suffix? x)    (list-ref x 5))
(define (dispatch:append? x)    (list-ref x 6))
(define (dispatch:op x)         (list-ref x 7))

(define (op-dispatch cbuf port
		     addr-list addr-cur addr-last
		     bmark-callback regex-callback)
  (log-debug-locals)
  (and-let* ((c (log-debug-pk (read-char port)))
	     (op (log-debug-pk (%op-key c)))
	     (addr (log-debug-pk (%op-addr op addr-list addr-cur addr-last)))
	     (special (log-debug-pk (%op-special port (dispatch:parser op))))
	     (suffix (log-debug-pk (%op-suffix port)))
	     (append (%op-append port)))
    
    ((dispatch:op op) cbuf addr special suffix append)))

(define (%op-key c)
  (let ((op (assoc c dispatch-table)))
    (unless op
      (set! *op-dispatch-error*
	(format #f "unknown command ~s" c)))
    op))

;; Arguments
;;
;; If an operation takes zero addresses but receives more than
;; zero addresses, it is an error.

;; If an operation takes more than zero addresses but receives
;; zero addresses, the defaults are used.

;; If an operation takes two addresses but receives one
;; address, apparently, the given address is used for both.
(define (%op-addr op addr-list line-cur line-last)
  (let ((addr-count-required (dispatch:addr-count op))
	(addr-list-len (length addr-list)))
    (cond
     ((and (= addr-count-required 0) (> addr-list-len 0))
      (set! *op-dispatch-error*
	(format #f "command ~s expects zero addresses" (dispatch:key op)))
      #f)
     (else
      (let ((addr
	     (cond
	      ((= addr-count-required 0)
	       '())
	      ((= addr-count-required 1)
	       (cond
		((>= addr-list-len 1)
		 (list (last addr-list)))
		(else
		 (list (convert-default-addr (dispatch:addr-start op) line-cur line-last)))))
	      ((= addr-count-required 2)
	       (cond
		((= addr-list-len 2)
		 addr-list)
		((= addr-list-len 1)
		 (list (last addr-list)
		       (last addr-list)))
		((= addr-list-len 0)
		 (list (convert-default-addr (dispatch:addr-start op) line-cur line-last)
		       (convert-default-addr (dispatch:addr-end op) line-cur line-last)))))
	      (else
	       ;; Should never reach here.
	       (set! *op-dispatch-error*
		 (format #f "command ~a expects ~a addresses but received ~a addresses"
			 (dispatch:key op) addr-count-required addr-list-len))
	       #f))))
	addr)))))

(define (%op-special port parser)
  "Some commands require parsing of addition information on the command-line
after the command character."
  (cond
   ((eqv? parser 'null)
    "")
   ((eqv? parser 'bmark)
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)
   ((eqv? parser 'file)
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)
   ((eqv? parser 'address)
    #f)
   ((eqv? parser 'regex)
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)
   ((eqv? parser 'regex+cmd)
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)
   ((eqv? parser 'regex+replace)
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)
   ((eqv? parser 'shell)
    #f)
   (else
    (set! *op-dispatch-error* (format #f "unhandled parser type ~a" parser))
    #f)))

(define (%op-suffix port)
  "Some commands allow 'l' 'n' or 'p' afterward to print a result."
  (let loop ((ret (peek-char-safe port))
	     (out ""))
    (if (member ret (string->list "lnp"))
	(begin
	  (read-char port)
	  (loop (peek-char-safe port) (string-append out (string ret))))
	;; else
	out)))

(define (%op-append port)
  "Some commands allow the entry of text lines, ended by entering
a single '.' on its own line"
  ;; (drain-input port)
  (let loop ((out '())
	     (line (read-line (current-input-port))))
    (log-debug-locals)
    (cond
     ((eof-object? line)
      out)
     ((string=? line ".")
      out)
     (else
      (loop (append out (list line))
	    (read-line (current-input-port)))))))

(define (convert-default-addr sym line-cur line-last)
  (cond
   ((integer? sym)
    sym)
   ((eqv? 'dot)
    line-cur)
   ((eqv? '$)
    line-last)
   ((eqv? 'dot+1)
    (1+ line-cur))))

