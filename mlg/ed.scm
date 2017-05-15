;; Helper functions for ed-like editing
(define-module (mlg ed)
  #:use-module (ice-9 getopt-long)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 popen)
  #:use-module (gano CBuffer)
  #:use-module (ryu core)
  #:use-module (mlg utils)
  #:use-module (mlg re)
  #:use-module (mlg buf)
  #:use-module (mlg strings)
  #:use-module (mlg port)
  #:use-module (mlg debug)
  #:use-module (mlg ed address)
  #:use-module (mlg ed bmark)
  #:use-module (mlg ed regex)
  #:use-module (mlg ed ops)
  #:export ())

;; "static buffers"
(define errmsg #f)			; error message buffer
(define old-filename "")		; default filename
(define shcmd "")			; last executed shell command
;; "global buffers"
;;(define ibuf (make-string 0))		; ed command-line buffer
;;(define ibufp (open-input-string ibuf)) ; a port to handle the command line buf
(define cbuf #f)

;; global flags
(define garrulous #f)			; if set, print all error messages
(define isglobal #f)			; if set, doing a global command
(define modified #f)			; if set, buffer modified since last write
(define scripted #f)			; if set, suppress diagnostics
(define interactive #t)			; if set, we are in interactive mode
(define verbose #t)			; if set, we are verbose

;; signals
(define mutex 0)			; if set, signals set flags
(define sighup #f)			; if set, sighup received while mutex set
(define sigint #f)			; if set, sigint received while mutex set
(define sigactive #f)			; if set, signal handlers are enabled.

(define current-addr 0)			; current address in editor buffer
(define addr-last 0)			; last address in editor buffer
(define lineno 0)			; script line number
(define prompt-string "*")		; command-line prompt
(define prompt-active #f)
(define home "") 			; home directory

;; Globals
(define EOF -1)
(define ERR -2)
(define EMOD -3)
(define FATAL -4)

(define (INC_MOD L K)
  (if (> (1+ L) K)
      0
      (1+ L)))
(define (DEC_MOD L K)
  (if (< (1- L) 0)
      K
      (1- L)))

(define (seterrmsg msg)
  "Set the text of error messages to be printed."
  (set! errmsg msg))

(define (main args)
  (let* ((option-spec '((prompt (single-char #\p) (value #t) (predicate string?))
			(scripted (single-char #\s) (value #f))))
	 (options (getopt-long args option-spec))
	 (argv (option-ref options '() '()))
	 (status 0))

    (set! home (get-home-dir))
    
    (set! prompt-string (option-ref options 'prompt "*"))
    (set! scripted (option-ref options 'scripted #f))

    ;; If one of the remaining arguments is a sole hyphen,
    ;; some versions of ed make this scripted mode.
    
    ;; Determine if we're interactive.
    ;; isatty  | file-port?  |
    ;; yes     | yes         | interactive
    ;; no      | yes         | input is piped into
    ;; no      | no          | probably running under repl
    (set! interactive (or (and (isatty? (current-input-port))
			       (file-port? (current-input-port)))
			  (and (not (isatty? (current-input-port)))
			       (not (file-port? (current-input-port))))))
    ;; Signal handler installation goes here.
    (when (isatty? (current-input-port))
      (handle-winch SIGWINCH)
      (sigaction SIGWINCH handle-winch))
    (sigaction SIGHUP signal-hup)
    (sigaction SIGQUIT SIG_IGN)
    (sigaction SIGINT signal-int)

    ;; FIXME: This is the big interrupt catch.
    ;; If I catch a sigint, restart restart processing from here.

    ;; FIXME: this is where we make an empty buffer
    ;; (init-buffers)
    (setvbuf (current-input-port) _IONBF)
    (set! cbuf (make-empty-cbuffer))

    ;; Enable signal handlers
    (set! sigactive #t)
    
    ;; Load the file, if a filename is given
    (cond
     ((and (> (length argv) 0) (> (string-length (car argv)) 0))
      (let* ((fname (car argv))
	     (ret (read-file fname 0 current-addr scripted)))
	(if (and (< ret 0) (not interactive))
	    (quit 2)
	    ;; else
	    (if (not (string-starts-with? fname #\!))
		(set! old-filename fname)))))
     (else
      (format-error "?~%")
      (unless interactive
	(quit 2))))


    ;; Prepare the subsystems
    (bmark-set-default-cbuf cbuf)
    (regex-set-default-cbuf cbuf)
    
    ;; This is the main loop
    (while #t
      (when (and (or (not status) (< status 0)) garrulous)
	(format-error "~a~%" errmsg))
      (if verbose
	  (%dump-cbuffer cbuf))
      (when prompt-active (display prompt))
      (force-output)

      ;; This is input is parsed and commands are run
      (let ((line-cur (get-line-cur cbuf))
	    (line-last (max 0 (1- (get-line-count cbuf))))
	    (port (open-input-string (read-line (current-input-port)))))
	(let ((addr-range (addr-get-range port
					  line-cur line-last
					  bmark-default-cb regex-default-cb)))
	  (when (not addr-range)
	    (seterrmsg (addr-get-range-error))
	    (set! status ERR)
	    (continue))
	  (set! status
	    (op-dispatch cbuf port addr-range line-cur line-last
		       bmark-default-cb regex-default-cb))

	  ;; Read any unused character in this line
	  (if (not (eof-object? (peek-char port)))
	      (read-line (current-input-port))))))))
    #|
      (let ((n (get-tty-line)))
	(cond
	 ((< n 0)
	  (set! status ERR)
	  (continue))

	 ((= n 0)
	  (if (and (modified (not scripted)))
	      (begin
		(format-error "?~%")
		(seterrmsg "warning: file modified")
		(unless interactive
		  (when garrulous
		    (format-error "script, line ~a: ~a~%"
				  lineno errmsg))
		  (quit 2))
		;; FIXME: Is there a Guile equivalent C's clearerr() ?
		(set! modified #f)
		(set! status EMOD)
		(continue))
	      ;; else
	      (quit 0))))

	;; FIXME, do we need to handled an unexpected end of file?
	;; e.g. like when there is no terminal newline in a script.
	
	(set! isglobal #f)
	;; Here we parse the current command.  The exec-command
	;; call is where most of the operations magic happens.

	(set! status (extract-addr-range))
	(when (>= status 0)
	  (set! status (exec-command)))
	(cond
	  ((= status 0)
	   (continue))

	  ((> status 0)
	   ;; A non-zero is a description of the type of after-command
	   ;; print operation requested)
	   (set! status (display-lines current-addr current-addr status))
	   (continue))
	  
	  ;; A status of less than zero is handled here
	  ((= status EOF)
	   (quit 0))
	  
	  ((= status EMOD)
	   (set! modified #f)
	   (format-error "?~%")
	   (seterrmsg "warning: file modified")
	   (unless interactive
	     (when garrulous
	       (format-error "script, line ~a: ~a~%" lineno errmsg))
	     (quit 2)))
	  
	  ((= status FATAL)
	   (cond
	    ((not interactive)
	     (when garrulous
	       (format-error "script, line ~a: ~a~%" lineno errmsg)))
	    
	    (garrulous
	     (format-error "~a~%" errmsg)))
	   (quit 3))
	  
	  (else
	   (format-error "?~%")
	   (unless interactive
	     (when garrulous
	       (format-error "script, line ~a: ~a~%" lineno errmsg))
	     (quit 2)))))))
  0)

(define addr-cnt 0)
(define first-addr 0)
(define second-addr 0)

(define (extract-addr-range)
  "Get line addresses from the command buffer port until
an illegal address is seen. Return status."
  (let ((addr 0))
    (set! addr-cnt 0)
    (set! first-addr current-addr)
    (set! second-addr current-addr)

    ;; Loop over all the addresses that appear before the command
    ;; character in an ED command line.
    (while #t
      (set! addr (next-addr))
      (if (< addr 0)
	  (break))
      (++ addr-cnt)
      (pk 'extract-addr-range 'increment-addr-cnt addr-cnt)
      (set! first-addr second-addr)
      (set! second-addr addr)
      (cond
       ((and (not (peek-char=? ibufp #\,)) (not (peek-char=? ibufp #\;)))
	(break))
       ((read-char=? ibufp #\;)
	(set! current-addr addr))))

    (set! addr-cnt (min addr-cnt 2))
    
    (when (or (= 1 addr-cnt)
	      (!= second-addr addr))
      (set! first-addr second-addr))
    (if (= addr ERR)
	ERR
	0)))

;; IN: current-addr ibufp
(define (next-addr)
  "Return the next line address in the command buffer."
  (let ((addr current-addr)
	(n 0)
	(first #t)
	(c 0)
	(ret 0)
	(iter 0))
    (pk 'next-addr-read-whitespace (read-whitespace ibufp))
    (while #t
      (++ iter)
      (let ((c (peek-char ibufp)))
	(pk 'next-addr-loop ibufp c)
	(cond
	 ((eof-object? c)
	  (pk 'next-addr-loop-eof ibufp c)
	  (set! ret EOF)
	  (break))
	 
	 ((member c (string->list "+ -^\t"))
	  (pk 'next-addr-loop-pm ibufp c)
	  (read-char ibufp)
	  (read-whitespace ibufp)
	  (cond
	   ((peek-char-isdigit? ibufp)
	    (set! addr (* (read-integer ibufp)
			  (if (char=? c #\-) -1 1))))
	   ((peek-char-isspace? ibufp)
	    (set! addr (if (char=? c #\-) -1 1)))))
	 ;; and continue
	 
	 ((isdigit? c)
	  (pk 'next-addr-loop-isdigit ibufp c)
	  (unless first
	    (seterrmsg "invalid address")
	    (set! ret ERR)
	    (break))
	  (set! addr (read-integer ibufp)))
	 ;; and continue

	 ;; dot and dollar sign are aliases for current line and last
	 ;; line respectively.
	 ((member c (string->list ".$"))
	  (pk 'next-addr-loop-dot-dollar ibufp c)
	  
	  (unless first
	    (seterrmsg "invalid address")
	    (set! ret ERR)
	    (break))
	  (read-char ibufp)
	  (set! addr (if (char=? c #\.) current-addr addr-last)))
	 ;; and continue

	 ;; Slash and question mark denote forward regex search
	 ;; and backward regex search
	 ((member c (string->list "/?"))
	  (pk 'next-addr-loop-regex ibufp c)
	  (unless first
	    (seterrmsg "invalid address")
	    (set! ret ERR)
	    (break))

	  ;; FIXME, use read-regex-string to extract a regex.
	  ;; Try to compile it.
	  ;; Also, a single slash or ? can mean the last
					;regex
	  )

	 ;; Apostrophe followed by letter gets the line number
	 ;; of a bookmark.
	 ((char=? c #\')
	  (pk 'next-addr-loop-bookmark ibufp c)
	  (unless first
	    (seterrmsg "invalid address")
	    (set! ret ERR)
	    (break))
	  (read-char ibufp)
	  (unless (peek-char-islower? ibufp)
	    (seterrmsg "invalid mark character")
	    (set! ret ERR)
	    (break))
	  (let ((bm (ed-get-mark cbuf (read-char ibufp))))
	    (unless bm
	      (seterrmsg "unknown mark character")
	      (set! ret ERR)
	      (break))
	    
	    (set! addr bm)))

	 ;; When using semicolon as an address separator,
	 ;; a missing initial address is the current line.
	 ;; When using comma as an address separator, a missing
	 ;; initial address is the 1st line.
	 ((and first (member c (string->list ",;")))
	  (pk 'next-addr-loop-comma-semi ibufp c)
	  (read-char ibufp)
	  (++ addr-cnt)
	  (set! second-addr (if (char=? c #\;) current-addr 1))
	  (let ((nxt (next-addr)))
	    (if (< nxt 0)
		(set! addr addr-last)
		(set! addr nxt))))
	 
	 (else
	  ;; No (more) addresses found
	  (pk 'next-addr-loop-else 'iter iter 'ibuf ibufp 'c c 'addr-cnt addr-cnt 'addr addr 'addr-last addr-last 'ret ret)
	  (cond
	   ((= iter 1)
	    ;; The no address found
	    (set! ret EOF)
	    (break))
	   ((or (< addr 0) (< addr-last addr))
	    (seterrmsg "invalid address")
	    (set! ret ERR)
	    (break))
	   (else
	    (set! ret addr)
	    (break))))))
      (set! first #f))
    ret))

(define-syntax set&get
  (syntax-rules ()
    ((_ var x)
     (begin
       (set! var x)
       var))))

;; Gflags
;; Global command
(define GLB #x01)
;; Print after command
(define GPR #x02)
;; List after command
(define GLS #x04)
;; Enumerate after command
(define GNP #x08)
;; Global substitute
(define GNP #x10)

(define sgflag 0)
(define sgnum 0)

(define (SPL1)
  "Disable some interrupts"
  (set! mutex (1+ mutex)))
(define (SPL0)
  "Enable all interrupts and chack signal flags."
  (set! mutex (1- mutex))
  (when (= 0 mutex)
    (when sighup
      (handle-hup SIGHUP))
    (when sigint
      (handle-int SIGINT))))


(define cbuf #f)



(define (display-lines from to gflag)
  "Print a range of lines to stdout."
  (cond
   ((zero? from)
    (seterrmsg "invalid address")
    ERR)
   (else
    (do ((i from (1+ i)))
	((> i to))
      (put-tty-line (get-text-line cbuf (1- i)) i gflag))
    0)))

(define (put-tty-line str n gflag)
  "Print text to stdout."
  (if (logtest gflag GNP)
      (format #t "~d~/" n))
  (if (logtest gflag GLS)
      (format #t "~a$~%" (string->ed-escaped-string str))
      (format #t "~a~%" str)))

(define (get-marked-node-addr n)
  ;; FIXME: this is where I hook into CBuffer's bookmarks\
  (throw 'unimplemented)
  )

(define (get-matching-node-addr pat dir)
  (throw 'unimplemented)
  ERR)


(define (clear-undo-stack)
  (format #t "in clear-undo-stack UNIMPLEMENTED~%"))

(define (GET_COMMAND_SUFFIX)
  (format #t "Entering GET_COMMAND_SUFFIX~%")
  (let ((done #f)
	(gflag 0))
    (while (not done)
      (let ((c (peek-char ibufp)))
	(cond
	 ((eof-object? c)
	  (set! done #t))
	 ((char=? c #\p)
	  (set! gflag (logior gflag GPR))
	  (read-char ibufp))
	 ((char=? c #\l)
	  (set! gflag (logior gflag GLS))
	  (read-char ibufp))
	 ((char=? c #\n)
	  (set! gflag (logior gflag GNP))
	  (read-char ibufp))
	 (else
	  (set! done #t)))))
    (read-whitespace ibufp)
    (let ((c (peek-char ibufp)))
      (pk 'after-cmd-suffix 'gflag gflag 'c c)
      (if (not (eof-object? c))
	  (begin
	    (seterrmsg "invalid command suffix")
	    (format #t "Leaving GET_COMMAND_SUFFIX FAIL gflag = ~a~%" gflag)
	    #f)
	  (begin
	    (format #t "Leaving GET_COMMAND_SUFFIX SUCCESS gflag = ~a~%" gflag)
	    gflag)))))

(define (exec-command)
  "Execute the next command in the command buffer; return
print request, if any."
  (format #t "entering exec-command~%")
  (let* (
	 ;; (tpat #f)
	 (fnp 0)
	 (gflag 0)
	 ;; (sflags 0)
	 (addr 0)
	 ;; (n 0)
	 )
    (read-whitespace ibufp)
    (let ((c (peek-char ibufp)))
      (cond

       ;; Append command
       ((char=? c #\a)
	(read-char ibufp)
	(let ((suffix (GET_COMMAND_SUFFIX)))
	  (format #t "in append command suffix=~a~%" suffix)
	  (if suffix
	      (begin
		(unless isglobal
		  (clear-undo-stack))
		(let ((success (append-lines second-addr)))
		  (if (< success 0)
		      ERR
		      suffix)))
	      ;; else
	      ERR)))

       ;; Change command
       ((char=? c #\c)
	(cond
	 ((< (check-addr-range current-addr current-addr) 0)
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 (else
	  (unless isglobal (clear-undo-stack))
	  (if (or (< (delete-lines first-addr second-addr) 0)
		  (< (append-lines current-addr) 0))
	      ERR
	      ;; else
	      gflag))))

       ;; Delete command
       ((char=? c #\d)
	(cond
	 ((< (check-addr-range current-addr current-addr) 0)
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 (else
	  (unless isglobal (clear-undo-stack))
	  (if (< (delete-lines first-addr second-addr) 0)
	      ERR
	      ;; else
	      (if (!= (set&get addr (INC_MOD current-addr addr-last)) 0)
		  (begin
		    (set! current-addr addr)
		    gflag)
		  ;; else
		  gflag)))))

       ;; Edit command
       ((and (char=? c #\e) modified (not scripted))
	EMOD)

       ;; Edit without checking command
       ((or (and (char=? c #\e) (or (not modified) scripted))
	    (char=? c #\E))
	(cond
	 ((< addr-cnt 0)
	  (seterrmsg "unexpected address")
	  ERR)
	 ((not (peek-char-isspace? ibufp))
	  (seterrmsg "unexpected command suffix")
	  ERR)
	 ((not (set&get fnp (get-filename)))
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 ((< (delete-lines 1 addr-last) 0)
	  ERR)
	 (else
	  (clear-undo-stack)
	  (cond
	   ;; ((< (close-sbuf) 0) ERR)
	   ;; ((< (open-sbuf) 0) FATAL)
	   (else
	    (when (and (> (string-length fnp) 0) (not (string=? fnp "!")))
	      (set! old-filename fnp))
	    (cond
	     ((< (read-file (if (> (string-length fnp) 0)
				fnp
				old-filename)
			    0 current-addr scripted)
		 0)
	      ERR)
	     (else
	      (clear-undo-stack)
	      (set! modified 0)
	      ;; (set! u-current-addr -1)
	      ;; (set! u-addr-last -1)
	      )))))))

       ;; Filename command
       ((char=? c #\f)
	(cond
	 ;; No arguments allowed.
	 ((> addr-cnt 0)
	  (seterrmsg "unexpected address")
	  ERR)
	 ;; No suffix allowed.
	 ((not (peek-char-isspace? ibufp))
	  (seterrmsg "unexpected command suffix")
	  ERR)
	 ((not (set&get fnp (get-filename)))
	  ERR)
	 ((string=? fnp "!")
	  (seterrmsg "invalid redirection")
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  (seterrmsg "invalid command suffix")
	  ERR)
	 ((> (string-length fnp) 0)
	  (set! old-filename fnp)
	  (display (string-strip-escapes old-filename))
	  (newline))))

       ;; Search
       ((member c (string->list "gGvV"))
	(cond
	 (isglobal
	  (seterrmsg "cannot nest global commands")
	  ERR)
	 ((< (check-addr-range 1 addr-last) 0)
	  ERR)
	 ((< (build-active-list (or (char=? c #\g) (char=? c #\G))) 0)
	  ERR)
	 ((and (!= 0 (set&get n (or (char=? c #\G) (char=? c #\V))))
	       (not (GET_COMMAND_SUFFIX)))
	  ERR)
	 (else
	  (++ isglobal)
	  (if (< (exec-global n gflag) 0)
	      ERR
	      ;; else
	      gflag))))

       ;; Help command
       ((char=? c #\h)
	(cond
	 ((> addr-cnt 0)
	  (seterrmsg "unexpected address")
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 (errmsg
	  (format-error "~a~%" errmsg)))
	gflag)

       ;; Help-mode-command
       ((char=? c #\H)
	(cond
	 ((> addr-cnt 0)
	  (seterrmsg "unexpected address")
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 ((and (set&get garrulous (not garrulous)) errmsg)
	  (format-error "~a~%" errmsg)))
	gflag)

       ;; Insert command
       ((char=? c #\i)
	(when (= second-addr 0)
	  (set! second-addr 1))
	(cond
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 (else
	  (unless isglobal (clear-undo-stack))
	  (when (< (append-lines (1- second-addr)) 0)
	    ERR))))

       ;; Join command
       ((char=? c #\j)
	(cond
	 ((< (check-addr-range current-addr (1+ current-addr)) 0)
	  ERR)
	 ((not (GET_COMMAND_SUFFIX))
	  ERR)
	 (else
	  (unless isglobal (clear-undo-stack))
	  (if (and (!= first-addr second-addr)
		   (< (join-lines first-addr second-addr) 0))
	      ERR
	      ;; else
	      gflag))))

       ;; Mark command
       ((char=? c #\k)
	(let ((c2 (read-char ibufp)))
	  (cond
	   ((= 0 second-addr)
	    (seterrmsg "invalid address")
	    ERR)
	   ((not (GET_COMMAND_SUFFIX))
	    ERR)
	   ((< (mark-line-node (get-addressed-line-node second-addr)) 0)
	    ERR)
	   (else
	    gflag))))

       ;; Escaped print
       ((char=? c #\l)
	(cond
	 ((< (check-addr-range current-addr current-addr) 0)
	  ERR)
	 ((< (display-lines first-addr second-addr GLS) 0)
	  ERR)
	 (else
	  0)))

       ;; Basic print with line numbers
       ((char=? c #\n)
	(cond
	 ((< (check-addr-range current-addr current-addr) 0)
	  ERR)
	 ((< (display-lines first-addr second-addr GNP) 0)
	  ERR)
	 (else
	  0)))

       ;; Basic print
       ((char=? c #\p)
	(cond
	 ((< (check-addr-range current-addr current-addr) 0)
	  ERR)
	 ((< (display-lines first-addr second-addr GPR) 0)
	  ERR)
	 (else
	  0)))
	  
	  
       ;; Write command
       ((or (char=? c #\w) (char=? #\W))
	(read-char ibufp)
	(if (not (peek-char-isspace? ibufp))
	    (begin
	      (seterrmsg "unexpected command suffix")
	      ERR)
	    ;;
	    (let ((fnp (get-filename)))
	      (if (not fnp)
		  ERR
		  ;; else
		  (if (and (or (!= addr-cnt 0) (!= addr-last 0))
			   (< (check-addr-range 1 addr-last) 0))
		      ERR
		      ;; else
		      (begin
			(when (and (= addr-cnt 0) (= addr-last 0))
			  (set! first-addr 0)
			  (set! second-addr 0))
			(when (and (not (string-null? fnp))
				   (not (string-starts-with? fnp #\!)))
			  (set! old-filename fnp))
			(let ((addr (write-file
				     ;; If a filename wasn't given,
				     ;; save to the last know filename.
				     (if (string-null? fnp)
					 old-filename
					 fnp)
				     ;; If the command was uppercase W,
				     ;; we're appending the output to the
				     ;; file
				     (if (char=? c #\W)
					 "a"
					 "w")
				     first-addr
				     second-addr)))
			  (cond
			   ((< addr 0)
			    ERR)
			   ((and (= addr addr-last)
				 (not (string-starts-with? fnp #\!)))
			    (set! modified #t)
			    0)
			   (else
			    0)))))))))
       ))))

(define (check-addr-range n m)
  "Return status of address range check."
  (when (= addr-cnt 0)
    (set! first-addr n)
    (set! second-addr m))
  (if (or (> first-addr second-addr)
	  (> 1 first-addr)
	  (> second-addr addr-last))
      (begin
	(pk 'check-addr-range-FAIL 'addr-cnt addr-cnt 'first-addr first-addr
	    'second-addr second-addr 'n n 'm m)
	(seterrmsg "invalid address")
	ERR)
      ;; else
      0))

(define (get-filename)
  (let ((filesz 0))
    (read-whitespace ibufp)
    (let ((filename (read-line ibufp)))
      (cond
       ((eof-object? filename)
	(seterrmsg "invalid filename")
	#f)
       ((string-starts-with? filename #\!)
	(let ((status (get-shell-command filename)))
	  (cond
	   ((< status 0)
	    #f)
	   ((> status 0)
	    (format #t "~a~%" shcmd)
	    (string-append (string #\!) shcmd))
	   (else
	    (string-append (string #\! shcmd))))))
       (else
	filename)))))

(define (get-shell-command _str)
  "Strip backslash escapes. Replace '%' with the current
filename. Replace '!' with the last executed shell command."
  (let* ((stripped-str (string-strip-escapes _str))
	 (ret 0)
	 (shell-str (string-fold
		     (lambda (c prev)
		       (cond
			((char=? c #\!)
			 (if (string-null? shcmd)
			     (begin
			       (seterrmsg "no previous command")
			       (set! ret ERR))
			     ;; else
			     (set! ret 1)))
			(string-append prev shcmd)
			((char=? c #\%)
			 (if (string-null? old-filename)
			     (begin
			       (seterrmsg "no current filename")
			       (set! ret ERR))
			     ;;
			     (set! ret 1))
			 (string-append prev old-filename))
			(else
			 (string-append prev (string c)))))
		     ""
		     stripped-str)))
    (if (>= ret 0)
	(set! shcmd shell-str))
    ret))

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

(define (delete-lines from to)
  (SPL1)
  (ed-delete cbuf from to)
  (set! modified #t)
  (set! addr-last (get-line-count cbuf))
  (set! current-addr (1- from))
  (SPL0)
  0)

(define (append-lines n)
  "Insert text from stdin to after line N. Stop when either a
single period is read or EOF.  Return status."
  (format #t "in append-lines n=~s~%" n)
  (let ((L 0)
	(lp ibufp)
	(eot #\null)
	(up #f)
	(ret 0))
    (set! current-addr n)
    (while #t
      (cond

       ;; If this is a regular append, and not part of a global
       ;; command, then get a line from the console.
       ((not isglobal)
	(cond
	 ((< (set&get L (get-tty-line)) 0)
	  (set! ret ERR)
	  (break))
	 (else
	  (set! lp 0))))

       ;; If this is part of a global command, but there is
       ;; no text left in the command buffer, we're done.
       ((char=? #\null (string-ref-safe ibufp (set&get lp ibufp)))
	0)

       ;; Else, if this is part of a global command, and there
       ;; is text left in the command buffer, figure out how much
       ;; text is left.
       (else
	(while (not (read-char=? #\null (ibufp*++))))
	(set! L (- ibufp lp))))

      ;; Now, either the text is '.' meaning quit, or
      ;; it is a line to be inserted
      (if (and (= L 1)
	       (char=? #\. (string-ref-safe ibuf lp)))
	  (begin
	    (SPL1)
	    (ed-append cbuf n (list (read-string ibufp)))
	    (set! ret 0)
	    (set! modified #t)
	    (SPL0)
	    (break))))
    (set! addr-last (get-line-count cbuf))
    ret))

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


;; a (append) + suffix
;; c (change) + suffix
;; d (delete) + suffix
;; e (edit) + blanks + filename-or-!
;; E (edit wo checking) + blanks + filename-or-!
;; f (filename) + blanks + filename
;; g (global) + regex + command list
;; G (global) + regex + suffix
;; h (help) + suffix
;; H (help-mode) + suffix
;; i (insert) + suffix
;; j (join) + suffix
;; k (mark) + bookmark + suffix
;; l (list) + suffix
;; m (move) + address + suffix
;; n (number) + suffix
;; p (print)  + suffix
;; P (prompt) + suffix
;; q (quit)
;; Q (quit w/o checking)
;; r (read) + blanks + filename-or-!
;; s (substitute) + regex + replacement + flags
;; t (copy) + address + suffix
;; u (undo) + suffix
;; v (non-match) + regex + command-list
;; V (non-match) + regex + suffix
;; w (write) + blanks + filename-or-!
;; = (line-no) + suffix
;; ! (shell) + command

(define (read-file fn n current-addr scripted)
  "Read a named file/pipe into the buffer.  Return line count."

  ;; This whole function is a bit garbage, because they tried to hard
  ;; to merge the popen and fopen into one path.
  (let ((size 0)
	(port 
	 (if (string-starts-with? fn #\!)
	     (open-input-pipe (string-drop fn 1))
	     ;; else
	     (false-if-exception (open-input-file (string-strip-escapes fn))))))
    
    (cond
     ((not port)
      (seterrmsg "cannot open input file")
      ERR)
     ((< (set&get size (ed-edit cbuf port)) 0)
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

(define (write-file filename mode n m)
  (let ((size 0)
	(port 
	 (if (string-starts-with? filename #\!)
	     (open-output-pipe (string-drop filename 1))
	     ;; else
	     (false-if-exception (open-file (string-strip-escapes filename) mode)))))
    
    (cond
     ((not port)
      (seterrmsg "cannot open output file")
      ERR)
     ((< (set&get size (ed-write cbuf n m port)) 0)
      ERR)
     ((if (char=? (string-ref-safe filename 0) #\!)
	  (not (status:exit-val (close-pipe port)))
	  ;; else
	  (begin (close-output-port port) #t))
      (seterrmsg "cannot close output file")
      ERR)
     (else
      (unless scripted
	(format (current-error-port) "~a~%" size))
      (if (= n 0)
	  0
	  (+ m (- n) 1))))))
|#

(define (handle-winch signo)
  ;; When there is a way to check TIOCGWINSZ,
  ;; then set rows and cols here.
  *unspecified* )


(define (handle-hup signo)
  (unless sigactive
    (quit 1))				; signal race?
  (set! sighup #f)
  ;; Try to write the crash-out file here.  Or, failing that, in my
  ;; home directory.
  (if (and (not (zero? addr-last))
	   (< (write-file "ed.hup" "1" 1 addr-last) 0)
	   (not (string-null? home))
	   (string-starts-with? home #\/))
      (write-file (string-append home "/ed.hup") "w" 1 addr-last))
  (primitive-_exit 2))

(define (handle-int signo)
  (unless sigactive
    (primitive-_exit 1))
  (set! sigint #f)
  ;; FIXME, here I somehow jump to the top of the main loop
  (throw 'interrupt))

(define (signal-int signo)
  (if (> mutex 0)
      (set! sigint #t)
      (handle-int signo)))

(define (signal-hup signo)
  (if (> mutex 0)
      (set! sighup #t)
      (handle-hup signo)))

(main (command-line))
