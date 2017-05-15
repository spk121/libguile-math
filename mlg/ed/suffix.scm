(define-module (mlg ed suffix)
  #:use-module (mlg port)
  #:export (SUFFIX_PRINT
	    SUFFIX_LIST
	    SUFFIX_NUMBER
	    read-command-suffix))

;; Print after command
(define SUFFIX_PRINT #x02)
;; List after command
(define SUFFIX_LIST #x04)
;; Enumerate after command
(define SUFFIX_NUMBER #x08)


(define (read-command-suffix port)
  (let loop ((flags 0))
    (let ((c (peek-char port)))
      (cond
       ((eof-object? c)
	flags)

       ((char=? c #\p)
	(read-char port)
	(loop (logior flags SUFFIX_PRINT)))

       ((char=? c #\l)
	(read-char port)
	(loop (logior flags SUFFIX_LIST)))

       ((char=? c #\n)
	(read-char port)
	(loop (logior flags SUFFIX_NUMBER)))

       (else
	(read-whitespace port)
	flags)))))
