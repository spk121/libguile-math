(define-module (mlg bytevectors)
  #:use-module (ice-9 optargs)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (srfi srfi-1)
  #:export (bytevector-u8-map
	    bytevector-u8-map!
	    bytevector-u8-for-each
	    bytevector-u8-fold
	    bytevector-u8-map-to-list
	    bytevector-list-length
	    bytevector-list-append
	    read-bytevector))

(define *block-size* (* 16 1024))
(define *maximum-read* #xffffffffffffffff)

(define* (bytevector-u8-map proc bv #:optional (start 0) (end #f))
  "Given a bytevector BV and a procedure PROC that takes an
integer from 0 to 255 and returns an integer from 0 to 255, the
procedure is applied over BV and a new bytevector is returned.
If START and END are given, only a subsection of the bytevector
is used as input."
  (unless end
    (set! end (bytevector-length bv)))
  (let ((output (make-bytevector (- end start))))
    (do ((i start (1+ i)))
	((>= i end))
      (bytevector-u8-set! output (- i start)
			  (proc (bytevector-u8-ref bv i))))
    output))

(define* (bytevector-u8-map!  proc bv #:optional (start 0) (end #f))
  "Given a bytevector BV and a procedure PROC that takes an
integer from 0 to 255 and returns an integer from 0 to 255, the
procedure is applied over BV and the bytevector is modified in place.
If START and END are given, only a subsection of the bytevector
is processed.

The return value is unspecified."
  (unless end
    (set! end (bytevector-length bv)))
  (do ((i start (1+ i)))
      ((>= i end))
    (bytevector-u8-set! bv i
			(proc (bytevector-u8-ref bv i)))))

(define* (bytevector-u8-for-each proc bv #:optional (start 0) (end #f))
  "Given a bytevector BV and a procedure PROC that takes an integer
from 0 to 255, the procedure is applied over BV in left-to-right
order.  If START and END are given, only a subsection of the
bytevector is used as input.

The return value is unspecified."
  (unless end
    (set! end (bytevector-length bv)))
  (do ((i start (1+ i)))
      ((>= i end))
    (proc (bytevector-u8-ref bv i))))

(define* (bytevector-u8-fold proc bv init #:optional (start 0) (end #f))
  "Apply PROC to the elements of the bytevector BV to build a result.

Each PROC call takes two parameters.  The 1st parameter is a number
from 0 to 255 taken from the bytevector.  The 2nd parameter is the
return from the previous call to PROC, or the given INIT for the first
call to PROC.

If START and END are given, only a subsection of the bytevector is
used as input.

If the bytevector is of length zero, just INIT is returned."
  (unless end
    (set! end (bytevector-length bv)))
  (let ((result init))
    (do ((i start (1+ i)))
	((>= i end))
      (set! result (proc (bytevector-u8-ref i result))))
    result))

(define* (bytevector-u8-map-to-list proc bv #:optional (start 0) (end #f))
  "Apply PROC to the elements of the bytevector BV to build a result.
PROC is a procedure that takes a number from 0 to 255 and returns
a value, which need not be an integer.  A list of the return values
is returned.

If START and END are given, only a subsection of the bytevector is
used as input.

If the bytevector is of length zero, an empty list is returned."
  (unless end
    (set! end (bytevector-length bv)))
  (let ((result '()))
    (do ((i start (1+ i)))
	((>= i end))
      (set! result (append! result
			    (list (proc (bytevector-u8-ref i result))))))
    result))

(define (bytevector-list-length lst)
  "Return the total number of bytes in the contents of a list of
bytevectors."
  ;; assert list of bytevectors
  (if (null-list? lst)
      0
      ;; else
      (apply + (map bytevector-length lst))))

(define (bytevector-list-append lst)
  "Return a new bytevector whose contents are the appended contents of
a list of bytevectors."
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

(define (bytevector-u8->escaped-string-full
	 bv
	 start
	 end
	 c0-transformer
	 g0-transformer
	 del-transformer
	 c1-transformer
	 g1-transformer)
  "Convert the bytevector BV to a string using five transfomer
routines.

C0-TRANSFORMER must be a function that takes an integer between
0 and 31 and returns a string.

G0-TRANSFORMER takes 32 to 126 and returns a string.

DEL-TRANSFORMER takes the number 127 and returns a string.

C1-TRANSFORMER takes 128 to 159.

G1-TRANSFOMER takes 160 to 255."
  (string-concatenate
   (bytevector-u8-map-to-list
    (lambda (x)
      (cond
       ((<= 0 x 31)
	(c0-transformer x))
       ((<= 32 x 126)
	(g0-transformer x))
       ((= 127)
	(del-transformer x))
       ((<= 128 x 159)
	(c1-transformer x))
       ((<= 160 x 255)
	(g1-transformer))))
    bv
    start
    end)))

