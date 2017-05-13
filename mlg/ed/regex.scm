(define-module (mlg ed regex)
  #:use-module (gano CBuffer)
  #:export (regex-set-default-cbuf
	    get-regex-cbuffer
	    regex-default-cb))

(define *regex-cbuffer* #f)
(define *regex-string* ".")

(define (get-regex-cbuffer)
  "Return the CBuffer currently used for searching
regular expressions."
  *regex-cbuffer*)

(define (regex-set-default-cbuf cbuf)
  "Set the CBuffer to be used when searching regular
expressions."
  (set! *regex-cbuffer* cbuf))

(define (regex-default-cb regex-string start-line direction)
  "Does a regular expression search in the currently set
*regex-cbuffer*.  If direction is +1 it searches forward in the list
of strings starting with the line after start-line.  If direction is
-1 it searches backward staring with the line preceding start-line.
The search will wrap around the end of the list upto and including
start-line.

If regex-string is the empty string, the regex-string used in the
previous call will be used.

On success, it will return the line number where the match was found.
On failure, it will return #f."
  (if (not (string-null? regex-string))
      (set! *regex-string* regex-string))
  
  (if (= 1 direction)
      (ed-regex-search-forward-line *regex-cbuffer* start-line *regex-string*)
      ;; else
      (if (= -1 direction)
	  (ed-regex-search-backward-line *regex-cbuffer* start-line *regex-string*))))

