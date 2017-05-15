(define-module (mlg ed ops)
  #:use-module (mlg ed address)
  #:export (op-get-dispatch-error
	    op-dispatch))

(define *op-dispatch-error* "")

(define (op-get-dispatch-error)
  *op-dispatch-error*)

(define dispatch-table
 ;; 1. shortcut
 ;; 2. addresses
 ;; 3,4. default-addresses 1 and 2
 ;; 7. get filename
 ;; 8. special parser
 ;; 5. standard suffix
 ;; 6. get append lines
 `((#\a    1 .   #f null          #t #t  ,op-append)
  (#\c    2 .   .   null          #t #t  ,op-change)
  (#\d    2 .   .   null          #t #f  ,op-delete)
  (#\e    0 #f  #f  file          #f #f  ,op-edit)
  (#\E    0 #f  #f  file          #f #f  ,op-edit-without-checking)
  (#\f    0 #f  #f  file          #f #f  ,op-filename)
  (#\g    2 1   $   regex+cmd     #t #f  ,op-global)
  (#\G    2 1   $   regex         #t #f  ,op-global-interactive)
  (#\h    0 #f  #f  null          #t #f  ,op-help)
  (#\H    0 #f  #f  null          #t #f  ,op-help-mode)
  (#\i    1 .   #f  null          #t #t  ,op-insert)
  (#\j    2 .   .+1 null          #t #f  ,op-join)
  (#\k    1 .   #f  bmark         #t #f  ,op-mark)
  (#\l    2 .   .   null          #t #f  ,op-list)
  (#\m    2 .   .   address       #t #f  ,op-move)
  (#\n    2 .   .   null          #t #f  ,op-number)
  (#\p    2 .   .   null          #t #f  ,op-print )
  (#\P    0 #f  #f  null          #t #f  ,op-prompt)
  (#\q    0 #f  #f  null          #f #f  ,op-quit)
  (#\Q    0 #f  #f  null          #f #f  ,op-quit-without-checking)
  (#\r    1 $   #f  file          #f #f  ,op-read)
  (#\s    2 .   .   regex+replace #t #f  ,op-substitute)
  (#\t    2 .   .   address       #t #f  ,op-copy)
  (#\u    0 #f  #f  null          #t #f  ,op-undo)
  (#\v    2 1   $   regex+cmd     #t #f  ,op-global-non-matched)
  (#\V    2 1   $   regex         #t #f  ,op-global-interactive-non-matched)
  (#\w    2 1   $   file          #f #f  ,op-write)
  (#\=    1 $   #f  null          #t #f  ,op-line-number)
  (#\!    0 #f  #f  shell         #f #f  ,op-shell-escape)
  (#\null 1 .+1 #f  null          #t #f  ,op-null)))

(define (dispatch:key x)        (list-ref x 0))
(define (dispatch:addr-count x) (list-ref x 1))
(define (dispatch:addr-start x) (list-ref x 2))
(define (dispatch:addr-end x)   (list-ref x 3))
(define (dispatch:parser x)     (list-ref x 4))
(define (dispatch:suffix? x)    (list-ref x 5))
(define (dispatch:append? x)    (list-ref x 6))
(define (dispatch:op x)         (list-ref x 7))

(define (convert-default-addr sym line-cur line-last)
  (cond
   ((integer? sym)
    sym)
   ((eqv? '.)
    line-cur)
   ((eqv? '$)
    line-last)
   ((eqv? '.+1)
    (1+ line-cur))))

(define (op-dispatch cbuf port addr-range bmark-callback regex-callback)
  (let ((c (peek-char-safe port))
	(addr-range-len (length addr-range))
	(op (assoc c dispatch-table)))
    (cond
     ((not op)
      (set! *op-dispatch-error*
	(format #f "unknown command ~s" c))
      #f)
     (else
      (let ((addr-requirement (dispatch:addr-count op))
	    (parser (dispatch:parser op)))

	;; Arguments
	;;
	;; If an operation takes zero addresses but receives more than
	;; zero addresses, it is an error.
	
	;; If an operation takes more than zero addresses but receives
	;; zero addresses, the defaults are used.
	
	;; If an operation takes two addresses but receives one
	;; address, apparently, the given address is used for both.
	(cond
	 ((and (= addr-requirement 0) (> addr-range-len 0))
	  (set! *op-dispatch-error*
	    (format #f "command ~s expects zero addresses" c)
	    #f))
	 (else
	  (let ((addr
		 (cond
		  ((= addr-requirement 0)
		   '())
		  ((= addr-requirement 1)
		   (list (last addr-range)))
		  ((= addr-requriement 2)
		   (cond
		    ((= addr-range-len 2)
		     addr-range)
		    ((= addr-range-len 1)
		     (list (last addr-range)
			   (last addr-range)))
		    ((= addr-range-len 0)
		     (list (convert-default-addr (dispatch:addr-start op) line-cur line-last)
			   (convert-default-addr (dispatch:addr-end op) line-cur line-last)))))
		  (else
		   ;; Should never reach here.
		   (error
		    (format #f "command ~s expects ~a addresses but received ~a addresses"
			    c addr-requriement addr-range-len)))))
		(special
		 (cond
		  ((= parser 'null)
		   "")
		  ((= parser 'file)
		   (bmark-read))
		  ((= parser 'address)
		   #f)
		  ((= parser 'regex)
		   #f)
		  ((= parser 'regex+cmd)
		   #f)
		  ((= parser 'regex+replace)
		   #f)
		  ((= parser 'shell)
		   #f)
		  (else
		   )))
		(suffix
		 (if (dispatch:suffix? op)
		     ;; get suffix
		     #f))
		(append-lines
		 (if (dispatch:append? op)
		     ;; get append lines
		     #f)))
	    ((dispatch:op op) addr special suffix append-lines)))))))))

(define (op-append lineno)
  (get-command-suffix)
  (if (not isglobal)
      (clear-undo-stack))
  (append-lines (last address)))


;;;;;;;;;;;;;;;;
;; HELPER PROCEDURES

(define (append-lines-from-string-list)
  "Append a list of strings to the current buffer after line L."
  )

(define (append-lines-from-input-port)
  "Read lines from the input port, appending them to the current
buffer after line L.  A line containing a single period ends
the appending operation."
  )

(define (delete-lines LStart LEnd))

(define (change-lines-from-string-list))

(define (change-lines-from-input-port))

(define (exec-global))

(define (join-lines))

(define (split-lines))

(define (display-lines))

(define (read-file))

(define (search-and-replace))

(define (copy-lines))

(define (move-lines))

(define (undo))

(define (write-file))

(define ())

#!
static int
append_lines(int n)
{
	int l;
	char *lp = ibuf;
	char *eot;
	undo_t *up = NULL;

	for (current_addr = n;;) {
		if (!isglobal) {
			if ((l = get_tty_line()) < 0)
				return ERR;
			else if (l == 0 || ibuf[l - 1] != '\n') {
				clearerr(stdin);
				return  l ? EOF : 0;
			}
			lp = ibuf;
		} else if (*(lp = ibufp) == '\0')
			return 0;
		else {
			while (*ibufp++ != '\n')
				;
			l = ibufp - lp;
		}
		if (l == 2 && lp[0] == '.' && lp[1] == '\n') {
			return 0;
		}
		eot = lp + l;
		SPL1();
		do {
			if ((lp = put_sbuf_line(lp)) == NULL) {
				SPL0();
				return ERR;
			} else if (up)
				up->t = get_addressed_line_node(current_addr);
			else if ((up = push_undo_stack(UADD, current_addr,
			    current_addr)) == NULL) {
				SPL0();
				return ERR;
			}
		} while (lp != eot);
		modified = 1;
		SPL0();
	}
	/* NOTREACHED */
	}
!#

(define (get-tty-line)
  "Read a line of text from stdin into the command line buffer.
Return line length."
  (let ((txt (read-line (current-input-port))))
    (cond
     ((eof-object? txt)
      (seterrmsg "cannot read stdin")
      ERR)
     (else
      (set! ibuf txt)
      (set! ibufp (open-input-string ibuf))
      (++ lineno)
      (string-length txt)))))

(define (append-lines nstart)
  "Insert text from stdin to after line N. Stop when either a
single period is read or EOF.  Return status."
  (cond
   ((not isglobal)
    (SPL1)
    (let loop ((ncur nstart) (L (get-tty-line)))
      (if (< L 0)
	  ERR
	  ;; else, test for ".", which terminates.
	  (if (and (peek-char=? ibufp #\.)
		   (char=? (peek-2nd-char-safe ibufp) #\null))
	      (begin
		(when (> ncur nstart)
		  (set! modified #t)
		  (set! addr-last (get-line-count cbuf)))
		(set! current-addr (ed-line-number cbuf))
		(SPL0)
		0)
		  
	      ;; else, append this
	      (begin
		(ed-append cbuf ncur (list (read-string ibufp)))
		(loop (1+ ncur) (get-tty-line)))))))
   (else
    ;; Is global, so read text from the rest of the current
    ;; command line
    (ed-append cbuf nstart (read-string ibufp))
    0)))
