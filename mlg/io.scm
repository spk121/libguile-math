(define-module (mlg io)
  #:use-module (ice-9 popen)
  #:use-module (ryu core)
  #:use-module (ryu strings)
  #:export (read-file))

(define (read-file fn n current-addr scripted)
  "Read a named file/pipe into the buffer.  Return line count."

  ;; This whole function is a bit garbage, because they tried to hard
  ;; to merge the popen and fopen into one path.
  (let ((size 0)
	(port 
	 (if (string-start-with? fn #\!)
	     (open-input-pipe (string-drop fn 1))
	     ;; else
	     (false-if-exception (open-input-file (strip-escapes fn))))))
    
    (cond
     ((not port)
      (seterrmsg "cannot open input file")
      ERR)
     ((< (set&get size (read-stream port n)) 0)
      ERR)
     ((if (char=? (string-ref-safe fn 0) #\!)
	  (not (status:exit-val (close-pipe port)))
	  ;; else
	  (begin (close-input-port port) #t))
      (seterrmsg "cannot close input file")
      ERR)
     (else
      (unless scripted
	(format (current-error-port) "~a~%" size))
      (- current-addr n)))))

