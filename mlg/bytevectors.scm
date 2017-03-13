(define-module (mlg bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export (bytevector-list-length
	    bytevector-list-append
	    read-bytevector))

(define *block-size* (* 16 1024))
(define *maximum-read* #xffffffffffffffff)

(define (bytevector-list-length lst)
  "Return the number of bytes in a list of bytevectors."
  ;; assert list of bytevectors
  (if (null-list? lst)
      0
      ;; else
      (apply + (map bytevector-length lst))))

(define (bytevector-list-append lst)
  "Return a new bytevector whose contents are the appended contents of
the input bytevectors."
  ;; assert list of bytevector of length
  (if (null-list? lst)
      (make-bytevector 0)
      ;; else
      (let ([output (make-bytevector (bytevector-list-length lst))])
	(let loop ([entry (car lst)]
		   [rest (cdr lst)]
		   [count 0])
	  (bytevector-copy! entry 0
			    output count (bytevector-length entry))
	  (if (null-list? rest)
	      ;; Since there are no more entries, we're done.
	      output
	      ;; Otherwise, process the next entry.
	      (loop (car rest)
		    (cdr rest)
		    (+ count (bytevector-length entry))))))))
   
(define* (read-bytevector #:optional (port (current-input-port))
			 (count *maximum-read*))
  "Read all of the characters out of PORT and return them as a
bytevector.  If PORT is not supplied, use the current input port.  If
COUNT is provided, treat it as the maximum number of bytes to read."
  (let ([n 0]
	[buf (make-bytevector *block-size* 0)])
    (let loop ([bytes-to-read (min (- count n) *block-size*)]
	       [output (make-bytevector 0)])
      (let ([bytes-read (get-bytevector-n! port buf 0 bytes-to-read)])
	(if (eof-object? bytes-read)
	    ;; We're done. Return the complete output
	    output
	    ;; Not done yet. Append this block to output.
	    (let* ([output-len (bytevector-length output)]
		   [new-output (make-bytevector (+ output-len bytes-read))])
	      (bytevector-copy! output 0
				new-output 0 output-len)
	      (bytevector-copy! buf 0
				new-output output-len (+ output-len bytes-read))
	      (set! n (+ n bytes-read))
	      (if (> (- count n) 0)
		  ;; Still more to read; loop again.
		  (loop (min (- count n) *block-size*)
			new-output)
		  ;; Otherwise, we've reached COUNT bytes, so stop.
		  new-output)))))))
    
