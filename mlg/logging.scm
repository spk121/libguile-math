(define-module (mlg logging)
  #:export(logging-init))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From (ryu core)
(define (bytes-to-bits b)
  (* 8 b))

(define (unsigned-limit b)
  (1- (expt 2 (bytes-to-bits b))))

(define-inlinable (lognot-uint x b)
  (- (unsigned-limit b) (logand (unsigned-limit b) x)))

(define (lognot-uint16 x)
  "Find the bitwise complement of a 16-bit unsigned integer."
  (lognot-uint x 2))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; gmessages.h (55): GLogLevelFlags

;; Internal flags
(define LOG_FLAG_RECURSION (ash 1 0))
(define LOG_FLAG_FATAL     (ash 1 1))
;; Log level for errors and assertion failures
(define LOG_LEVEL_ERROR    (ash 1 2))
;; Log level for serious warnings
(define LOG_LEVEL_CRITICAL (ash 1 3))
(define LOG_LEVEL_WARNING  (ash 1 4))
(define LOG_LEVEL_MESSAGE  (ash 1 5))
;; For informational messages
(define LOG_LEVEL_INFO     (ash 1 6))
(define LOG_LEVEL_DEBUG    (ash 1 7))
;; Mask for all log levels but no internal flags
(define LOG_LEVEL_MASK     (lognot-uint16 (logior LOG_FLAG_RECURSION
						  LOG_FLAG_FATAL)))

;; Mask for log levels that are considered fatal by default
(define LOG_FATAL_MASK    (logior LOG_FLAG_RECURSION LOG_LEVEL_ERROR))

;; 1139: ALERT_LEVELS
(define ALERT_LEVELS      (logior LOG_LEVEL_ERROR
				  LOG_LEVEL_CRITICAL
				  LOG_LEVEL_WARNING))

;; 1142: DEFAULT_LEVELS
;; These are emitted by the default log handler.
(define DEFAULT_LEVELS    (logior LOG_LEVEL_ERROR
				  LOG_LEVEL_CRITICAL
				  LOG_LEVEL_WARNING
				  LOG_LEVEL_MESSAGE))
;; 1144 INFO_LEVELS
;; These are filtered by MESSAGES_DEBUG in the default log handler
(define INFO_LEVELS       (logior LOG_LEVEL_INFO
				  LOG_LEVEL_DEBUG))

;; 486: GLogDomain
(define-record-type <Domain>
  (make-domain log-domain fatal-mask handlers)
  domain?
  (_log_domain domain:log-domain)
  (_fatal_mask domain:fatal-mask domain:set-fatal-mask!)
  (_handlers domain:handlers domain:set-handlers!))

;; 493: GLogHandler
(define-record-type <LogHandler>
  (make-log-handler id log-level log-func destroy-notify)
  log-handler?
  (_id id)
  (_log_level log-level)
  (_log_func log-func)
  (_destroy-notify destroy-notify))

;; 505: variables
(define *messages-lock* (make-mutex))
(define *log-domains* '())
(define *log-depth* (make-fluid 0))
(define *log-structured-depth*  (make-fluid 0))
(define *fatal-log-func* #f)
(define *log-writer-func* #f)

;; 523: aka _g_log_abort
(define (%log-abort breakpoint)
  (if breakpoint
      (trap-here-0)
      (exit 1)))

;; 584: write_string
(define (write-string port string)
  "For now, an alias for display."
  (display string port))

;; 591: write_string_sized
(define (write-bytevector port bv)
  "For now, an alias for put-bytevector."
  (put-bytevector port bv))

;; 603: g_log_find_domain_L
(define (log-find-domain-L log-domain)
  "Finds a log domain whose domain string matches LOG-DOMAIN."
  (if (null? *log-domains*)
      #f
      (find
       (lambda (domain)
	 (equal? log-domain (domain:log-domain domain)))
       *log-domains*)))

;; 618: g_log_domain_new_L
(define (log-domain-new-L log-domain)
  "Appends a new default domain to the domain list.
Should this be in a mutex?  Should we be checking for duplicates?"
  (set! *log-domains*
    (append *log-domains*
	    (list (make-log-domain (string-copy log-domain)
				   LOG_FATAL_MASK
				   #f)))))

;; 634: g_log_domain_check_free_L
(define (log-domain-check-free-L domain)
  "This appears to be a routine to cull a given log domain from the
log domain list if it would never print anything."
  (when (and (eqv? (domain:fatal-mask domain) LOG_FATAL_MASK)
	     (null? (domain:handlers domain)))
    (set! *log-domains*
      (filter-map
       (lambda (entry)
	 (not (equal? domain entry)))
       *log-domains*))))

;; 663: g_log_domain_get_handler_L
(define (log-domain-get-handler-L domain log-level)
  "Searches through the log domain list to find a log function for a
given domain and log level."
  (when (and (domain? domain) log-level (not (zero? log-level)))
    (let* ((handlers (domain:handlers domain))
	   (handler (find (lambda (entry)
			    (eqv? (logand (handler:log-level entry) log-level)
				  log-level))
			  handlers)))
      (if handler
	  handler
	  *default-log-func*))))

;; 712: g_log_set_always_fatal
(define (log-set-always-fatal _fatal_mask)
  (let ((fatal-mask (logand (logior _fatal_mask LOG_LEVEL_ERROR)
			    (lognot-uint16 LOG_FLAG_FATAL))))
    (mutex-lock *messages-lock*)
    (let ((old-mask *log-always-fatal*))
      (set! *log-always-fatal* fatal-mask)
      (mutex-unlock *messages-lock*)
      old-mask)))

;; 750: g_log_set_fatal_mask
(define (log-set-fatal-mask _log_domain _fatal_mask)
  (let ((log-domain (or _log_domain ""))
	(fatal-mask (logand (logior _fatal_mask LOG_LEVEL_ERROR)
			    (lognot-uint16 LOG_FLAG_FATAL))))
    (mutex-lock *messages-lock*)
    (let* ((domain (or (log-find-domain-L log-domain)
		      (log-domain-new-L log-domain)))
	   (old-flags (domain:fatal-mask domain)))
      (domain:set-fatal-mask! domain fatal-mask)
      (log-domain-check-free-L domain)
      (mutex-unlock *messages-lock*)
      old-flags)))

;; 780: g_log_set_handler
(define (log-set-handler log-domain log-levels log-func)
  "Assign a new handler for a given domain and log level mask."
  (log-set-handler-full log-domain log-levels log-func #f))

;; 854: g_log_set_handler_full
(define (log-set-handler-full _log_domain log-levels log-func destroy)
  "Assign a new handler for a given domain and log level mask."
  (when (and (not (zero? (logand log-levels LOG_LEVEL_MASK)))
	     (procedure? log-func))
    
    (mutex-lock *messages-lock*)
    (set! *handler-id* (1+ *handler-id*))
    
    (let* ((log-domain (if (and _log_domain (not (string-null? _log_domain)))
			   (string-copy _log_domain)
			   ""))
	   (domain (or (log-find-domain-L log-domain)
		       (log-domain-new-L log-domain)))
	   (handler (make-log-handler *handler-id*
				      log-levels
				      log-func
				      destroy)))
      (domain:set-handlers! (list handler))
      (mutex-unlock *messages-lock*)
      ;; and return the unique ID
      (handler:id handler))))

;; 909: g_log_set_default_handler
(define (log-set-default-handler log-func)
  "Assign log-func as the default log handler function for non-fatal
cases."
  (mutex-lock *messages-lock*)
  (let ((old-log-func *default-log-func*))
    (set! *default-log-func* log-func)
    (mutex-unlock *messages-lock*)
    old-log-func))

;; 953: g_test_log_set_fatal_handler
(define (log-set-default-handler log-func)
  "Assign log-func as the default log handler function for fatal
cases."
  (mutex-lock *messages-lock*)
  (set! *fatal-log-func* log-func)
  (mutex-unlock *messages-lock*)
  *unspecified*)


;; 973: g_log_remove_handler
(define (log-remove-handler _log_domain handler-id)
  (when (> handler-id 0)
    (let ((log-domain (or (and _log_domain (not (string-null? _log_domain)))
			  "")))
      (mutex-lock *messages-lock*)
      (let ((domain (log-find-domain-L log-domain)))
	(
	 ;; HEREIAM:
    

(define (log-level->port log-level)
  (if (logtest log-level
	       (logior LOG_LEVEL_ERROR
		       LOG_LEVEL_WARNING
		       LOG_LEVEL_CRITICAL
		       LOG_LEVEL_MESSAGE))
      (current-error-port)
      (current-output-port)))



;; 1151: mklevel_prefix
(define (mklevel-prefix log-level use-color?)
  (string-append
   (log-level->color log-level use-color?)
   (cond
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_ERROR)
     "ERROR")
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_CRITICAL)
     "CRITICAL")
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_WARNING)
     "WARNING")
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_MESSAGE)
     "Message")
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_INFO)
     "INFO")
    ((eqv? (logand log-level LOG_LEVEL_MASK) LOG_LEVEL_DEBUG)
     "DEBUG")
    (else
     (if (not (zero? log-level))
	 (format #f "LOG-" (logand log-level LOG_LEVEL_MASK))
	 "LOG")))
   (color-reset use-color?)
   (if (logtest log-level LOG_FLAG_RECURSION)
       " (recursed)"
       "")
   (if (logtest log-level ALERT_LEVELS)
       " **"
       "")))

;; 1216: expected_messages
(define *expected-messages* '())

;; 1239: g_logv
(define (logv log-domain _log-level msg)
  (let ((was-fatal (logtest _log-level LOG_FLAG_FATAL))
	(was-recursion (logtest _log-level LOG_FLAG_RECURSION))
	(log-level (logand _log-level LOG_LEVEL_MASK)))
  (if (zero? log-level)
      #f
      ;; else
      (do ((i 8 (1- i)))
	  ((< i 0))
	(when (logtest (ash 1 i) log-level)
	  (let ((test-level (logior (ash 1 i)
				    (if was-fatal LOG_FLAG_FATAL 0)
				    (if was-recursion LOG_FLAG_RECURSION 0)))
		(log-func #f)
		(domain-fatal-mask #f))
	    
	    (lock-mutex *messages-lock*)
	    (let* ((depth (fluid-ref *log-depth*))
		   (domain (log-find-domain-L (if log-domain
						  log-domain
						  "")))
		   (test-level (if (zero? depth)
				   test-level
				   (logior test-level LOG_FLAG_RECURSION)))
		   (domain-fatal-mask (if domain
					  (domain:fatal-mask domain)
					  LOG_FATAL_MASK)))
	      (set! depth (1+ depth))
	      (when (logtest (logior domain-fatal-mask *log-always-fatal*) test-level)
		(set! test-level (logior test-level LOG_FLAG_FATAL)))
	      (if (logtest test-level LOG_FLAG_RECURSION)
		  (set! log-func %log-fallback-handler)
		  ;; else
		  (set! log-func (log-domain-get-handler-L domain test-level)))
	      (set! domain #f)
	      (unlock-mutex *messages-lock*)
	      (fluid-set! *log-depth* depth)

	      (log-func log-domain test-level msg)

	      (if (and (logtest test-level LOG_FLAG_FATAL)
		       (not (logtest test-level LOG_LEVEL_ERROR)))
		  (or (and fatal-log-func
			   (not (fatal-log-func log-domain test-level msg)))
		      (%log-abort (not (logtest test-level LOG_FLAG_RECURSION)))))

	      (set! depth (1- depth))
	      (fluid-set! *log-depth* depth))))))))
		  
	  

;; 1390: g_log
(define (log log-domain log-level formatstr . args)
  (if (null? args)
      (logv log-domain log-level formatstr)
      ;; else
      (logv log-domain log-level
	    (apply format (append (list #f formatstr) args)))))

;; 1436: log_level_to_color
(define (log-level->color log-level use-color?)
  (cond
   ((not use-color?)
    "")
   ((logtest log-level LOG_LEVEL_ERROR)
    "\033[1;31m")
   ((logtest log-level LOG_LEVEL_CRITICAL)
    "\033[1;35m")
   ((logtest log-level LOG_LEVEL_WARNING)
    "\033[1;33m")
   ((logtest log-level LOG_LEVEL_MESSAGE)
    "\033[1;32m")
   ((logtest log-level LOG_LEVEL_INFO)
    "\033[1;32m")
   ((logtest log-level G_LOG_LEVEL_DEBUG)
    "\033[1;32m")
   (else
    "")))

;; 1461: color_reset
(define (color-reset use-color?)
  (if use-color?
      "\033[0m"
      ""))

;; 1906: g_log_structured_array
(define (log-alist log-level fields-alist)
  (unless (null? fields-alist)
    (let* ((depth (fluid-ref *log-structured-depth*))
	   (recursion (> depth 0))
	   (writer-func #f))
      (lock-mutex *messages-lock*)
      (set! writer-func (if recursion
			    %log-writer-fallback
			    *log-writer-func*))
      (unlock-mutex *messages-lock*)

      (fluid-set! *log-structured-depth* (1+ depth))
      (when (procedure? writer-func)
	(writer-func log-level fields-alist))
      (fluid-set! *log-structured-depth* depth)

      (when (logtest log-level LOG_FATAL_MASK)
	  (%log-abort (not (logand log-level LOG_FLAG_RECURSION)))))))

;; 2168: g_log_writer_format_fields
(define (log-writer-format-fields log-level fields-alist use-color?)
  ;; Extract some common fields.
  (let ((message (assoc-ref fields-alist "MESSAGE"))
	(log-domain (assoc-ref fields-alist "DOMAIN"))
	(level-prefix (mklevel-prefix log-level use-color?))
	(gstring ""))
    (when (logtest log-level ALERT_LEVELS)
      (set! gstring (string-append gstring "\n")))
    (when (not log-domain)
      (set! gstring (string-append gstring "** ")))

    (when (eqv? (zero? (logand *log-msg-prefix* (logand log-level LOG_LEVEL_MASK)))
		(zero? (logand log-level LOG_LEVEL_MASK)))
      (let ((prg-name (get-prgname))
	    (pid (getpid)))
	(if (string-null? prg-name)
	    (set! gstring (string-append gstring (format #f "(process:~a): " pid)))
	    (set! gstring (string-append gstring (format #f "(~a:~a): " prg-name pid))))))

    (when log-domain
      (set! gstring (string-append gstring log-domain "-")))

    (set! gstring (string-append gstring ": "))
    (if (not message)
	(set! gstring (string-append gstring "(NULL) message"))
	;; else
	(set! gstring (string-append gstring message)))

    message))


;; 2358: g_log_writer_journald
(define (log-writer-journald log-level fields-alist)
  "This would send the alist of fields to the journal
using the sd_journal_send function.  The key is transformed
to a string in the portable character set.  The val
is transformed either into UTF-8 text or, if it is a bytevector,
into a binary blob."
  #f)

;; 2473: g_log_writer_standard_streams
(define (log-writer-standard-streams log-level fields-alist)
  (let ((port (log-level->port log-level)))
    (display (log-writer-format-fields log-level fields-alist #t) ; FIXME, query if color is supported
	     port)
    #t))
  
;; 2542: g_log_writer_default
(define (log-writer-default log-level fields-alist)
  (unless (null? fields-alist)

    ;; If the log-level is not in the DEFAULT_LEVELS, it is only
    ;; logged if there is a DOMAIN key in the fields which matches a
    ;; string in the MESSAGES_DEBUG environment variable.
    (when (or (logtest log-level DEFAULT_LEVELS)
	      (equal? (assoc-ref fields-alist "DOMAIN") "all")
	      (false-if-exception (string-contains (assoc-ref fields-alist "DOMAIN")
						   (getenv "MESSAGES_DEBUG"))))
      ;; We try to send to the journald first, or then fallback to the
      ;; standard streams.
      (if (and (not (and (log-writer-is-journald? (fileno (current-error-port)))
			 (log-writer-journald log-level fields-alist)))
	       (not (log-writer-standard-streams log-level fields-alist)))
	  ;; Looks like we failed to log anything normally, give up.
	  #f
	  ;; else
	  (begin
	    ;; We logged something.  Now we abort if
	    ;; the log was a fatal error.
	    (when (or (logtest log-level LOG_FATAL_MASK)
		      (logtest log-level *log-always-fatal*))
	      (%log-abort (not (logand (logior log-level LOG_FLAG_FATAL) LOG_FLAG_RECURSION))))
	    #t)))))

;; 2619: _g_log_writer_fallback
(define (%log-writer-fallback log-level fields-alist)
  "The most basic log message logging function.  It logs known keys to
the current error or output port."
  (let ((port (log-level->port log-level)))
    (for-each
     (lambda (entry)
       (let ((key (car entry))
	     (val (cdr entry)))
	 (if (member key '("MESSAGE"
			   "MESSAGE_ID"
			   "PRIORITY"
			   "CODE_FILE"
			   "CODE_LINE"
			   "CODE_FUNC"
			   "ERRNO"
			   "SYSLOG_FACILITY"
			   "SYSLOG_IDENTIFIER"
			   "SYSLOG_PID"
			   "GLIB_DOMAIN"))
	     (format port "~a=~s~%" key val))))
     fields-alist)
    (format port "_PID=~s~%" (getpid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logging-init)
  (set! *log-writer-func* log-writer-default))

(logging-init)
