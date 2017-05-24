(define-module (mlg logging)
  #:use-module (ice-9 format)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (system repl repl)
  #:use-module (system repl debug)
  #:use-module (system vm frame)
  #:use-module (rnrs bytevectors)
  #:use-module (rnrs io ports)
  #:use-module (mlg assert)
  #:use-module (mlg debug)
  #:use-module (mlg time)
  #:use-module (mlg utils)
  #:use-module (mlg journal)
  #:export(
	   __FILE__
	   __LINE__
	   __FUNC__
	   __LOCALS__

	   set-log-domain
	   get-log-domain
	   log-set-default-writer
	   log-enable-color
	   
	   log-structured
	   log-structured-alist

	   log-error
	   log-error-full
	   log-critical
	   log-warning
	   log-message
	   log-info
	   log-debug
	   
	   warn-if-false
	   warn-val-if-false
	   warn-if-used
	   warn-if-reached

	   log-debug-locals
	   log-debug-pk
	   log-debug-time
	   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From (ryu core)
(define (bytes-to-bits b)
  (assert-type integer b)
  (* 8 b))

(define (unsigned-limit b)
  (assert-type integer b)
  (1- (expt 2 (bytes-to-bits b))))

(define-inlinable (lognot-uint x b)
  (assert-type integer b)
  (assert-type integer x)
  (- (unsigned-limit b) (logand (unsigned-limit b) x)))

(define (lognot-uint16 x)
  (assert-type integer x)
  "Find the bitwise complement of a 16-bit unsigned integer."
  (lognot-uint x 2))

(define-syntax __FILE__
  (syntax-rules ()
    ((_)
     (if (assv-ref (current-source-location) 'filename)
	 (basename (assv-ref (current-source-location) 'filename))
	 ;; else
	 "(unknown file)"))))

(define-syntax __LINE__
  (syntax-rules ()
    ((_)
     (or (assv-ref (current-source-location) 'line)
	 -1))))

(define-syntax __FUNC__
  (syntax-rules ()
    ((_)
     (let ((stk (stack->vector (make-stack #t))))
       (let loop ((i 1))
	 (cond
	  ((and (frame-procedure (vector-ref stk i))
		(procedure-name (frame-procedure (vector-ref stk i))))
	   (let ((pname (procedure-name (frame-procedure (vector-ref stk i)))))
	     (cond
	      ((eqv? pname '%start-stack)
	       "(top level)")
	      ((eqv? pname 'save-module-excursion)
	       "(top level)")
	      (else
	       (symbol->string pname)))))
	  ((< i (vector-length stk))
	   (loop (1+ i)))
	  (else
	   "(unknown func)")))))))

(define-syntax STRLOC
  (syntax-rules ()
    ((_)
     (let ((loc (assv-ref (current-source-location) 'line)))
       (if loc
	   (number->string loc)
	   "(unknown line)")))))
#|
(define* (stringify-local-vars frame #:key (width 158) (per-line-prefix "  "))
  (let ((bindings (frame-bindings frame)))
    (cond
     ((null? bindings)
      (format #f "~aNo local variables.~%" per-line-prefix))
     (else
      (apply string-append 
	     (map
	      (lambda (binding)
		(format #f "~a~a = ~v:@y~%"
			per-line-prefix
			(binding-name binding)
			width
			(binding-ref binding)))
	      (frame-bindings frame)))))))
|#

(define-syntax __LOCALS__
  (syntax-rules ()
    ((_)
     (let ((stk (stack->vector (make-stack #t)))
	   (out "...\n"))
       (let loop ((i 1))
	 (let* ((frame (vector-ref stk i))
		(name (and (frame-procedure frame) 
			   (procedure-name (frame-procedure frame)))))
	   (set! out
	     (string-append out
			    (with-output-to-string
			      (lambda ()
				(print-locals frame
					      #:width 160
					      #:per-line-prefix "**   ")))))
	   (if (and (not name) (< i (vector-length stk)))
	       (loop (1+ i))
	       ;; else
	       out)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; From glib-init.c

(define *log-msg-prefix* #f)
(define *log-always-fatal* #f)

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

(define LOG_DOMAIN #f)

(define (set-log-domain str)
  (if (string? str)
      (set! LOG_DOMAIN str)
      (set! LOG_DOMAIN #f)))

(define (get-log-domain)
  LOG_DOMAIN)

;; gmessage.h: 246: G_DEBUG_HERE
(define-syntax log-debug-locals
  (syntax-rules ()
    ((_ ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_DEBUG
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" (__LOCALS__)))))

(define-syntax log-debug-time
  (syntax-rules ()
    ((_ ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_DEBUG
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" "Time is ~6,5f"
		     (* 1e-6 (- (monotonic-time) *log-start-time*))))))

;; gmessages.h: 285: g_error
(define-syntax log-error
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_ERROR
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 293: g_message
(define-syntax log-message
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_MESSAGE
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 298: g_critical
(define-syntax log-critical
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_CRITICAL
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 303: g_warning
(define-syntax log-warning
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_WARNING
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 308: g_info
(define-syntax log-info
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_INFO
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 313: g_debug
(define-syntax log-debug
  (syntax-rules ()
    ((_ msg ...)
     (log-structured LOG_DOMAIN LOG_LEVEL_DEBUG
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" msg ...))))

;; gmessage.h: 491: g_warn_if_reached

(define-syntax warn-if-reached
  (syntax-rules ()
    ((_)
     (log-structured LOG_DOMAIN LOG_LEVEL_CRITICAL
		     "CODE_FILE" (__FILE__)
		     "CODE_LINE" (STRLOC)
		     "CODE_FUNC" (__FUNC__)
		     "MESSAGE" "code should not be reached"))))

(define-syntax warn-if-used
  (syntax-rules ()
    ((_ val)
     (begin
       (log-structured LOG_DOMAIN LOG_LEVEL_CRITICAL
		       "CODE_FILE" (__FILE__)
		       "CODE_LINE" (STRLOC)
		       "CODE_FUNC" (__FUNC__)
		       "MESSAGE" "value '~a' should not be used"
		       val)
       val))))

(define-syntax warn-if-false
  (syntax-rules ()
    ((_ expr)
     (let ((ret expr))
       (or ret
	   (begin
	     (log-structured LOG_DOMAIN LOG_LEVEL_CRITICAL
			     "CODE_FILE" (__FILE__)
			     "CODE_LINE" (STRLOC)
			     "CODE_FUNC" (__FUNC__)
			     "MESSAGE" "'~a' is false"
			     (quote expr))
	     #f))))))

(define-syntax warn-val-if-false
  (syntax-rules ()
    ((_ expr val)
     (let ((ret expr))
       (or ret
	   (begin
	     (log-structured LOG_DOMAIN LOG_LEVEL_CRITICAL
			     "CODE_FILE" (__FILE__)
			     "CODE_LINE" (STRLOC)
			     "CODE_FUNC" (__FUNC__)
			     "MESSAGE" "'~a' is false. '~a'='~a'"
			     (quote expr) (quote val) val)
	     #f))))))


;; gmessages.h: 576: g_return_if_fail

;; gmessage.h: 585: g_return_val_if_fail

;; gmessage.h: 594: g_return_if_reached

;; gmessage.h: 603: g_return_val_if_reached

(define-syntax log-debug-pk
  (syntax-rules ()
    ((_ expr)
     (let ((ret expr))
       (log-structured LOG_DOMAIN LOG_LEVEL_DEBUG
		       "CODE_FILE" (__FILE__)
		       "CODE_LINE" (STRLOC)
		       "CODE_FUNC" (__FUNC__)
		       "MESSAGE" "'~s' is '~s'"
		       (quote expr) ret)
       ret))))



;; gmessages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 486: GLogDomain
(define-record-type <Domain>
  (make-domain _log_domain _fatal_mask _handlers)
  domain?
  (_log_domain domain:log-domain)
  (_fatal_mask domain:fatal-mask domain:set-fatal-mask!)
  (_handlers domain:handlers domain:set-handlers!))

;; 493: GLogHandler
(define-record-type <LogHandler>
  (make-log-handler _id _log_level _log_func _destroy_notify)
  log-handler?
  (_id log-handler:id)
  (_log_level log-handler:log-level)
  (_log_func log-handler:log-func)
  (_destroy_notify log-handler:destroy-notify))

(define (%dump-handler handler)
  (assert-type log-handler handler)
  (format #t "handler:        ~a~%" handler)
  (format #t "id:             ~a~%" (log-handler:id handler))
  (format #t "level:          ~b~%" (log-handler:log-level handler))
  (format #t "destroy notify: ~a~%" (log-handler:destroy-notify handler)))

(define (%dump-log-domain domain)
  (assert-type domain domain)
  (format #t "domain:         ~a~%" domain)
  (format #t "log domain:     ~a~%" (domain:log-domain domain))
  (format #t "fatal mask:     ~b~%" (domain:fatal-mask domain))
  (do ((i 0 (1+ i)))
      ((>= i (length (domain:handlers domain))))
    (format #t "Handler #~a~%" (1+ i))
    (%dump-handler (list-ref (domain:handlers domain) i))))

;; 505: variables
(define *messages-lock* (make-mutex))
(define *log-domains* '())
(define *log-depth* (make-fluid 0))
(define *log-structured-depth*  (make-fluid 0))
(define *default-log-func* #f)
(define *fatal-log-func* #f)
(define *log-writer-func* #f)
(define *log-enable-color* #t)
(define *log-start-time* #f)

(define (%dump-log-globals)
  (format #t "log-depth: ~a~%" (fluid-ref *log-depth*))
  (format #t "log-structured-depth: ~a~%" (fluid-ref *log-structured-depth*))
  (format #t "default log func: ~a~%" *default-log-func*)
  (format #t "fatal log func: ~a~%" *fatal-log-func*)
  (format #t "log writer func: ~a~%" *log-writer-func*)
  (for-each (lambda (domain) (%dump-log-domain domain))
	    *log-domains*))

;; 523: aka _g_log_abort
(define (%log-abort breakpoint?)
  (assert-type boolean breakpoint?)
  (if breakpoint?
      (breakpoint 'log-abort)
      (exit 1)))

;; 584: write_string
(define (write-string port string)
  (assert-type string string)
  (assert-type output-port port)
  "For now, an alias for display."
  (display string port))

;; 591: write_string_sized
(define (write-bytevector port bv)
  (assert-type output-port port)
  (assert-type bytevector bv)
  "For now, an alias for put-bytevector."
  (put-bytevector port bv))

;; 603: g_log_find_domain_L
(define (log-find-domain-L log-domain-str)
  "Finds a log domain whose domain string matches LOG-DOMAIN-STR."
  (assert-type string log-domain-str)
  (if (null? *log-domains*)
      #f
      (find
       (lambda (domain)
	 (equal? log-domain-str (domain:log-domain domain)))
       *log-domains*)))

;; 618: g_log_domain_new_L
(define (log-domain-new-L log-domain-str)
  "Appends a new default domain to the domain list.
Should this be in a mutex?  Should we be checking for duplicates?"
  (assert-type string log-domain-str)
  (set! *log-domains*
    (append *log-domains*
	    (list (make-domain (string-copy log-domain-str)
			       LOG_FATAL_MASK
			       #f)))))

;; 634: g_log_domain_check_free_L
(define (log-domain-check-free-L domain)
  "This appears to be a routine to cull a given log domain from the
log domain list if it would never print anything."
  (assert-type domain domain)
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
  (if (or (not (domain? domain)) (not log-level) (zero? log-level))
      *default-log-func*
      ;; else
      (let* ((handlers (domain:handlers domain))
	     (handler (find (lambda (entry)
			      (eqv? (logand (log-handler:log-level entry) log-level)
				    log-level))
			    handlers)))
	(if handler
	    handler
	    *default-log-func*))))

;; 712: g_log_set_always_fatal
(define (log-set-always-fatal _fatal_mask)
  (let ((fatal-mask (logand (logior _fatal_mask LOG_LEVEL_ERROR)
			    (lognot-uint16 LOG_FLAG_FATAL))))
    (lock-mutex *messages-lock*)
    (let ((old-mask *log-always-fatal*))
      (set! *log-always-fatal* fatal-mask)
      (unlock-mutex *messages-lock*)
      old-mask)))

;; 750: g_log_set_fatal_mask
(define (log-set-fatal-mask _log_domain _fatal_mask)
  (let ((log-domain (or _log_domain ""))
	(fatal-mask (logand (logior _fatal_mask LOG_LEVEL_ERROR)
			    (lognot-uint16 LOG_FLAG_FATAL))))
    (lock-mutex *messages-lock*)
    (let* ((domain (or (log-find-domain-L log-domain)
		       (log-domain-new-L log-domain)))
	   (old-flags (domain:fatal-mask domain)))
      (domain:set-fatal-mask! domain fatal-mask)
      (log-domain-check-free-L domain)
      (unlock-mutex *messages-lock*)
      old-flags)))

;; 780: g_log_set_handler
(define (log-set-handler log-domain log-levels log-func)
  "Assign a new handler for a given domain and log level mask."
  (log-set-handler-full log-domain log-levels log-func #f))

;; 860: handler_id
(define *handler-id* 0)

;; 854: g_log_set_handler_full
(define (log-set-handler-full _log_domain log-levels log-func destroy)
  "Assign a new handler for a given domain and log level mask."
  (when (and (not (zero? (logand log-levels LOG_LEVEL_MASK)))
	     (procedure? log-func))
    
    (lock-mutex *messages-lock*)
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
      (domain:set-handlers! domain (list handler))
      (unlock-mutex *messages-lock*)
      ;; and return the unique ID
      (log-handler:id handler))))

;; 909: g_log_set_default_handler
(define (log-set-default-handler log-func)
  "Assign log-func as the default log handler function for non-fatal
cases."
  (lock-mutex *messages-lock*)
  (let ((old-log-func *default-log-func*))
    (set! *default-log-func* log-func)
    (unlock-mutex *messages-lock*)
    old-log-func))

(define (log-set-default-writer writer_func)
  "Set the function to be used for logging.  WRITER_FUNC is a procedure
that takes two parameters: a log-level integer, and an alist of key/value
pairs.

As a special case, if WRITER_FUNC is the symbol 'standard, the default
standard streams writer will be used.  If WRITER_FUNC is 'journal, 
the systemd journal writer will be used."
  (lock-mutex *messages-lock*)
  (cond
   ((eqv? writer_func 'standard)
    (set! *log-writer-func* log-writer-standard-streams))
   ((eqv? writer_func 'journal)
    (set! *log-writer-func* log-writer-journal))
   ((procedure? writer_func)
    (set! *log-writer-func* writer_func))
   (else
    (set! *log-writer-func* log-writer-standard-streams)))
  (unlock-mutex *messages-lock*))

(define (log-enable-color flag)
  "Given FLAG, which is #t or #f, this enables or disable colorization
when logging to the standard streams."
  (lock-mutex *messages-lock*)
  (set! *log-enable-color* flag)
  (unlock-mutex *messages-lock*))

;; 953: g_test_log_set_fatal_handler
(define (log-set-fatal-handler log-func)
  "Assign log-func as the default log handler function for fatal
cases."
  (lock-mutex *messages-lock*)
  (set! *fatal-log-func* log-func)
  (unlock-mutex *messages-lock*)
  *unspecified*)


;; 973: g_log_remove_handler
(define (log-remove-handler _log_domain handler-id)
  (when (> handler-id 0)
    (let ((log-domain (or (and _log_domain (not (string-null? _log_domain)))
			  ""))
	  (notify-func #f)
	  (notify-domain #f)
	  (notify-handler #f)
	  (warning #f))
      (lock-mutex *messages-lock*)
      (let ((domain (log-find-domain-L log-domain)))
	(when domain
	  ;; Remove any handler with this ID from the current log
	  ;; domain.
	  (let ((handler (find
			  (lambda (entry)
			    (eqv? (log-handler:id entry) handler-id))
			  (domain:handlers domain))))
	    (if handler
		(begin
		  (domain:set-handlers! domain (delete! handler (domain:handlers domain)))
		  (log-domain-check-free-L domain)
		  (when (log-handler:destroy-notify handler)
		    (set! notify-func (log-handler:destroy-notify handler))
		    (set! notify-domain domain)
		    (set! notify-handler handler)))
		;; else, if no handler found
		(set! warning (format #f "could not find handler with id '~a' for domain \"~a\"" handler-id log-domain)))))
	(unlock-mutex *messages-lock*)
	;; Shouldn't call notify notification func within mutex.
	(if notify-func
	    (notify-func notify-domain notify-handler))

	;; FIXME: later, make this a g-warning
	(if warning
	    (display warning (current-error-port)))))))

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
	 (format #f "LOG-~a" (logand log-level LOG_LEVEL_MASK))
	 "LOG")))
   (color-reset use-color?)
   (if (logtest log-level LOG_FLAG_RECURSION)
       " (recursed)"
       "")
   (if (logtest log-level ALERT_LEVELS)
       " **"
       "")))

;; 1150: extracted fro mmklevel_prefix
(define (log-level->port log-level)
  (if (logtest log-level
	       (logior LOG_LEVEL_ERROR
		       LOG_LEVEL_WARNING
		       LOG_LEVEL_CRITICAL
		       LOG_LEVEL_MESSAGE))
      (current-error-port)
      (current-output-port)))

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
		;;(pk 'test-level test-level 'LOG_FLAG_RECURSION LOG_FLAG_RECURSION 'domain domain)
		(if (logtest test-level LOG_FLAG_RECURSION)
		    (set! log-func %log-fallback-handler)
		    ;; else
		    (set! log-func (log-domain-get-handler-L domain test-level)))
		(set! domain #f)
		(unlock-mutex *messages-lock*)
		(fluid-set! *log-depth* depth)

		;;(pk 'log-func log-func 'log-domain log-domain)
		(log-func log-domain test-level msg)

		(if (and (logtest test-level LOG_FLAG_FATAL)
			 (not (logtest test-level LOG_LEVEL_ERROR)))
		    (or (and (procedure? *fatal-log-func*)
			     (not (*fatal-log-func* log-domain test-level msg)))
			(%log-abort (not (logtest test-level LOG_FLAG_RECURSION)))))

		(set! depth (1- depth))
		(fluid-set! *log-depth* depth))))))))

;; 1390: g_log
(define (%log log-domain log-level formatstr . args)
  (if (null? args)
      (logv log-domain log-level formatstr)
      ;; else
      (logv log-domain log-level
	    (apply format (append (list #f formatstr) args)))))

;; 1406: log_level_to_priority
(define (log-level->priority log-level)
  "A one-byte name for a log level."
  (cond
   ((logtest log-level LOG_LEVEL_ERROR)    "3")
   ((logtest log-level LOG_LEVEL_CRITICAL) "4")
   ((logtest log-level LOG_LEVEL_WARNING)  "4")
   ((logtest log-level LOG_LEVEL_MESSAGE)  "5")
   ((logtest log-level LOG_LEVEL_INFO)     "6")
   ((logtest log-level LOG_LEVEL_DEBUG)    "7")))

;; 1436: log_level_to_color
(define (log-level->color log-level use-color?)
  (cond
   ((not (and use-color? *log-enable-color*))
    "")
   ((logtest log-level LOG_LEVEL_ERROR)
    "\x1b[1;31m")
   ((logtest log-level LOG_LEVEL_CRITICAL)
    "\x1b[1;35m")
   ((logtest log-level LOG_LEVEL_WARNING)
    "\x1b[1;33m")
   ((logtest log-level LOG_LEVEL_MESSAGE)
    "\x1b[1;32m")
   ((logtest log-level LOG_LEVEL_INFO)
    "\x1b[1;32m")
   ((logtest log-level LOG_LEVEL_DEBUG)
    "\x1b[1;32m")
   (else
    "")))

;; 1461: color_reset
(define (color-reset use-color?)
  (if (and use-color? *log-enable-color*)
      "\x1b[0m"
      ""))

;; 1665: g_log_structured
(define (%add-fields-from-list-to-alist _output rest)
  (let loop ((key #f)
	     (cur (car rest))
	     (rest (cdr rest))
	     (output _output))
    (cond
     ((not key)
      (if (null? rest)
	  (begin
	    ;; We have a leftover key with no value.
	    output)
	  ;; Else, this element must be a key.
	  (loop cur (car rest) (cdr rest) output)))
     ((not (string=? key "MESSAGE"))
      (if (null? rest)
	  (begin
	    ;; We have a key with no value.
	    output)
	  ;; Else, this element must be the value of a key/value pair.
	  (begin
	    (set! output (assoc-set! output key cur))
	    (loop #f (car rest) (cdr rest) output))))
     (else
      ;; We have a "MESSAGE" key, so the remaining params are a format
      ;; specifier and its parameters.
      ;;(pk 'key key 'cur cur 'rest rest)
      (set! output (assoc-set! output key
			       (apply format (append (list #f)
						     (list cur)
						     rest))))
      output))))

(define (log-structured log-domain log-level . rest)
  "Log a message to structured data.  The message will be passed
through the lo writer set by the application using
log-set-writer-func.  If the message is fatal, the program will be
aborted.

For each key to log, the procedure takes two parameters: a key
string and a value string.

There must always be a MESSAGE key, which must be the last entry in
the list.  For that key, the variables that follow the MESSAGE key
will be merged into a single string using (format #f param1 param2 ...)"
  ;;(pk 'log-structured 'log-domain log-domain 'log-level log-level 'rest rest)
  (let ((fields-alist (acons "PRIORITY" (log-level->priority log-level) '())))
    ;;(pk 'fields-alist1 fields-alist)
    (when log-domain
      (set! fields-alist (acons "DOMAIN" log-domain fields-alist)))
    ;;(pk 'fields-alist2 fields-alist)
    (set! fields-alist (%add-fields-from-list-to-alist fields-alist rest))
    ;;(pk 'fields-alist3 fields-alist)
    (log-structured-alist log-level fields-alist)))

;; 1906: g_log_structured_array
(define (log-structured-alist log-level fields-alist)
  ;;(pk 'log-structured-alist 'log-level log-level 'fields-alist fields-alist)
  (unless (null? fields-alist)
    (let* ((depth (fluid-ref *log-structured-depth*))
	   (recursion (> depth 0))
	   (writer-func #f))
      (lock-mutex *messages-lock*)
      (set! writer-func (if recursion
			    %log-writer-fallback
			    *log-writer-func*))
      ;;(pk 'writer-func writer-func)
      (unlock-mutex *messages-lock*)

      (fluid-set! *log-structured-depth* (1+ depth))
      (when (procedure? writer-func)
	(writer-func log-level fields-alist))
      (fluid-set! *log-structured-depth* depth)

      (when (logtest log-level LOG_FATAL_MASK)
	(%log-abort (not (logand log-level LOG_FLAG_RECURSION)))))))

;; 2102: g_log_writer_is_journald
(define (log-writer-is-journald? fileno)
  #f)

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
    (set! gstring (string-append gstring level-prefix))
    
    (set! gstring (string-append gstring ": "))
    (let ((codefile (assoc-ref fields-alist "CODE_FILE"))
	  (codeline (assoc-ref fields-alist "CODE_LINE"))
	  (codefunc (assoc-ref fields-alist "CODE_FUNC")))
      (when codefile
	(set! gstring (string-append gstring codefile ":")))
      (when codeline
	(set! gstring (string-append gstring codeline ":")))
      (when codefunc
	(set! gstring (string-append gstring codefunc ":")))
      (when (or codefile codeline codefunc)
	(set! gstring (string-append gstring " "))))

    (if (not message)
	(set! gstring (string-append gstring "(NULL) message"))
	;; else
	(set! gstring (string-append gstring message)))
    gstring))

;; 2293: g_log_default_handler
(define (log-default-handler log-domain log-level message)
  (cond
   ((logtest log-level LOG_FLAG_RECURSION)
    (%log-fallback-handler log-domain log-level message))
   
   (else
    (let ((fields-alist
	   (append
	    `(("OLD_LOG_API" . "1")
	      ("MESSAGE" . ,message)
	      ("PRIORITY" . ,(log-level->priority log-level)))
	    (if log-domain
		`(("DOMAIN" . ,log-domain))
		'()))))
      (log-structured-alist (logand log-level (lognot-uint16 LOG_FLAG_FATAL))
			    fields-alist)))))

;; 2358: g_log_writer_journald
(define (log-writer-journal log-level fields-alist)
  "This would send the alist of fields to the journal using the
sd_journal_send function.  The key is transformed to a string in the
portable character set.  The val is transformed either into UTF-8 text
or, if it is a bytevector, into a binary blob."

  ;; FIXME: what if there is already a "PRIORITY"?
  (let ((alist (acons "PRIORITY" (log-level->priority log-level)
		      fields-alist)))
    (send-alist-to-journal alist)))

;; 2473: g_log_writer_standard_streams
(define (log-writer-standard-streams log-level fields-alist)
  ;;(pk 'log-writer-standard-streams log-level fields-alist)
  (let ((port (log-level->port log-level)))
    (display (log-writer-format-fields log-level fields-alist
				       (is-color-supported? port))
	     port)
    (newline port)
    (force-output port)
    #t))

(define (is-color-supported? port)
  ;; If the input port is not a tty or file port we are probably
  ;; running under the repl.
  ;; If the output port is not a tty and a file port, then we are
  ;; probably not running in a terminal.
  (and (isatty? (current-input-port))
       (file-port? (current-input-port))
       (isatty? port)
       (file-port? port)
       (getenv "TERM")
       (member (getenv "TERM")
	       '(
		 "386at"
		 "aixterm"
		 "aixterm-16color"
		 "amiga-vnc"
		 "ansi"
		 "ansi80x25"
		 "ansi80x25-raw"
		 "ansi80x30"
		 "ansi80x43"
		 "ansi80x50"
		 "ansi80x60"
		 "ansi-color-2-emx"
		 "ansi-color-3-emx"
		 "ansi-emx"
		 "ansil"
		 "ansis"
		 "ansi.sys"
		 "ansisysk"
		 "ansi.sysk"
		 "ansi.sys-old"
		 "ansiw"
		 "Apple_Terminal"
		 "arm100"
		 "arm100-am"
		 "arm100-w"
		 "arm100-wam"
		 "at386"
		 "atari-color"
		 "atari_st-color"
		 "at-color"
		 "aterm"
		 "att6386"
		 "beterm"
		 "bsdos-pc"
		 "bsdos-pc-nobold"
		 "bsdos-ppc"
		 "bterm"
		 "color_xterm"
		 "cons25"
		 "cons25-debian"
		 "cons25-iso8859"
		 "cons25-koi8-r"
		 "cons25l1"
		 "cons25r"
		 "cons25w"
		 "cons30"
		 "cons43"
		 "cons50"
		 "cons50-iso8859"
		 "cons50-koi8r"
		 "cons50l1"
		 "cons50r"
		 "cons60"
		 "cons60-iso"
		 "cons60-koi8r"
		 "cons60l1"
		 "cons60r"
		 "crt"
		 "crt-vt220"
		 "ctrm"
		 "cx"
		 "cx100"
		 "cygwin"
		 "cygwinB19"
		 "cygwinDBG"
		 "d220"
		 "d220-7b"
		 "d220-dg"
		 "d230"
		 "d230c"
		 "d230c-dg"
		 "d230-dg"
		 "d430c-dg"
		 "d430c-dg-ccc"
		 "d430c-unix"
		 "d430c-unix-25"
		 "d430c-unix-25-ccc"
		 "d430c-unix-ccc"
		 "d430c-unix-s"
		 "d430c-unix-s-ccc"
		 "d430c-unix-sr"
		 "d430c-unix-sr-ccc"
		 "d430c-unix-w"
		 "d430c-unix-w-ccc"
		 "d430-dg"
		 "d430-dg-ccc"
		 "d430-unix"
		 "d430-unix-25"
		 "d430-unix-25-ccc"
		 "d430-unix-ccc"
		 "d430-unix-s"
		 "d430-unix-s-ccc"
		 "d430-unix-sr"
		 "d430-unix-sr-ccc"
		 "d430-unix-w"
		 "d430-unix-w-ccc"
		 "d470"
		 "d470-7b"
		 "d470c"
		 "d470c-7b"
		 "d470c-dg"
		 "d470-dg"
		 "darwin"
		 "darwin-100x37"
		 "darwin-112x37"
		 "darwin-128x40"
		 "darwin-128x48"
		 "darwin-144x48"
		 "darwin-160x64"
		 "darwin-200x64"
		 "darwin-200x75"
		 "darwin-256x96"
		 "darwin-80x25"
		 "darwin-80x30"
		 "darwin-90x30"
		 "darwin-b"
		 "darwin-f"
		 "darwin-f2"
		 "decansi"
		 "dg+ccc"
		 "dg+color"
		 "dg+color8"
		 "dg+fixed"
		 "dgmode+color"
		 "dgmode+color8"
		 "dgunix+ccc"
		 "dgunix+fixed"
		 "djgpp"
		 "djgpp204"
		 "dtterm"
		 "ecma+color"
		 "emots"
		 "emu"
		 "emx-base"
		 "Eterm"
		 "Eterm-256color"
		 "Eterm-88color"
		 "eterm-color"
		 "Eterm-color"
		 "gnome"
		 "gnome-2007"
		 "gnome-2008"
		 "gnome-2012"
		 "gnome-256color"
		 "gnome-fc5"
		 "gnome-rh62"
		 "gnome-rh72"
		 "gnome-rh80"
		 "gnome-rh90"
		 "gs6300"
		 "hft"
		 "hft-c"
		 "hft-c-old"
		 "hft-old"
		 "hp2397"
		 "hp2397a"
		 "hp+color"
		 "hpterm-color"
		 "hurd"
		 "i3164"
		 "ibm+16color"
		 "ibm3164"
		 "ibm5081"
		 "ibm5154"
		 "ibm6154"
		 "ibm8503"
		 "ibm8507"
		 "ibm8512"
		 "ibm8513"
		 "ibm8514"
		 "ibm8604"
		 "ibm+color"
		 "ibmpc3"
		 "ibmpc3r"
		 "interix"
		 "interix-nti"
		 "iris-color"
		 "iterm"
		 "iTerm.app"
		 "jaixterm"
		 "jfbterm"
		 "klone+color"
		 "kon"
		 "kon2"
		 "konsole"
		 "konsole-16color"
		 "konsole-256color"
		 "konsole-base"
		 "konsole-linux"
		 "konsole-solaris"
		 "konsole-vt100"
		 "konsole-vt420pc"
		 "konsole-xf3x"
		 "konsole-xf4x"
		 "kterm"
		 "kterm-co"
		 "kterm-color"
		 "kvt"
		 "linux"
		 "linux-16color"
		 "linux2.2"
		 "linux2.6"
		 "linux2.6.26"
		 "linux3.0"
		 "linux-basic"
		 "linux-c"
		 "linux-c-nc"
		 "linux-koi8"
		 "linux-koi8r"
		 "linux-lat"
		 "linux-nic"
		 "linux-vt"
		 "mach-color"
		 "mach-gnu-color"
		 "mgt"
		 "mgterm"
		 "minitel1"
		 "minitel1b"
		 "minix"
		 "minix-3.0"
		 "mlterm"
		 "mlterm2"
		 "mlterm-256color"
		 "mlterm3"
		 "mrxvt"
		 "mrxvt-256color"
		 "ms-vt100+"
		 "ms-vt100-color"
		 "ms-vt-utf8"
		 "mvterm"
		 "nansisys"
		 "nansi.sys"
		 "nansisysk"
		 "nansi.sysk"
		 "ncr260intan"
		 "ncr260intpp"
		 "ncr260intwan"
		 "ncr260intwpp"
		 "ncr260wy325pp"
		 "ncr260wy325wpp"
		 "ncr260wy350pp"
		 "ncr260wy350wpp"
		 "ncsa"
		 "ncsa-ns"
		 "ncsa-vt220"
		 "netbsd6"
		 "nsterm"
		 "nsterm-16color"
		 "nsterm-256color"
		 "nsterm-7"
		 "nsterm-7-c"
		 "nsterm-7-c-s"
		 "nsterm-7-s"
		 "nsterm-acs"
		 "nsterm-acs-c"
		 "nsterm-acs-c-s"
		 "nsterm-acs-s"
		 "nsterm-bce"
		 "nsterm-build326"
		 "nsterm-build343"
		 "nsterm-build361"
		 "nsterm-c"
		 "nsterm+c"
		 "nsterm+c41"
		 "nsterm-c-7"
		 "nsterm-c-acs"
		 "nsterm-c-s"
		 "nsterm-c-s-7"
		 "nsterm-c-s-acs"
		 "nsterm-old"
		 "nsterm-s"
		 "nsterm-s-7"
		 "nsterm-s-acs"
		 "ntconsole"
		 "ntconsole-100"
		 "ntconsole-100-nti"
		 "ntconsole-25"
		 "ntconsole-25-nti"
		 "ntconsole-25-w"
		 "ntconsole-25-w-vt"
		 "ntconsole-35"
		 "ntconsole-35-nti"
		 "ntconsole-35-w"
		 "ntconsole-50"
		 "ntconsole-50-nti"
		 "ntconsole-50-w"
		 "ntconsole-60"
		 "ntconsole-60-nti"
		 "ntconsole-60-w"
		 "ntconsole-w"
		 "ntconsole-w-vt"
		 "nxterm"
		 "old-st"
		 "opennt"
		 "opennt-100"
		 "opennt-100-nti"
		 "opennt-25"
		 "opennt-25-nti"
		 "opennt-25-w"
		 "opennt-25-w-vt"
		 "opennt-35"
		 "opennt-35-nti"
		 "opennt-35-w"
		 "opennt-50"
		 "opennt-50-nti"
		 "opennt-50-w"
		 "opennt-60"
		 "opennt-60-nti"
		 "opennt-60-w"
		 "opennt-nti"
		 "opennt-w"
		 "opennt-w-vt"
		 "pc3"
		 "pc3-bold"
		 "pc3r"
		 "pcansi"
		 "pcansi25"
		 "pcansi-25"
		 "pcansi33"
		 "pcansi-33"
		 "pcansi43"
		 "pcansi-43"
		 "pccon"
		 "pccon0"
		 "pccon+colors"
		 "pc-minix"
		 "pcvt25-color"
		 "putty"
		 "putty-256color"
		 "putty-noapp"
		 "putty-sco"
		 "putty-vt100"
		 "qansi"
		 "qansi-g"
		 "qansi-m"
		 "qansi-t"
		 "qansi-w"
		 "qnx"
		 "qnx4"
		 "qnxm"
		 "qnxt"
		 "qnxt2"
		 "qnxt4"
		 "qnxw"
		 "rcons-color"
		 "rxvt"
		 "rxvt-16color"
		 "rxvt-256color"
		 "rxvt-88color"
		 "rxvt-color"
		 "rxvt-cygwin"
		 "rxvt-cygwin-native"
		 "rxvt-unicode"
		 "rxvt-xpm"
		 "scoansi"
		 "scoansi-new"
		 "scoansi-old"
		 "screen"
		 "screen-16color"
		 "screen-16color-bce"
		 "screen-16color-bce-s"
		 "screen-16color-s"
		 "screen-256color"
		 "screen-256color-bce"
		 "screen-256color-bce-s"
		 "screen-256color-s"
		 "screen-bce"
		 "screen-bce.Eterm"
		 "screen-bce.gnome"
		 "screen-bce.konsole"
		 "screen-bce.linux"
		 "screen-bce.mrxvt"
		 "screen-bce.rxvt"
		 "screen-bce.xterm-new"
		 "screen.Eterm"
		 "screen.gnome"
		 "screen.konsole"
		 "screen.konsole-256color"
		 "screen.linux"
		 "screen.mlterm"
		 "screen.mlterm-256color"
		 "screen.mrxvt"
		 "screen.putty"
		 "screen.putty-256color"
		 "screen.rxvt"
		 "screen-s"
		 "screen.teraterm"
		 "screen.vte"
		 "screen.vte-256color"
		 "screen-w"
		 "screen.xterm-256color"
		 "screen.xterm-new"
		 "screen.xterm-xfree86"
		 "simpleterm"
		 "st"
		 "st-16color"
		 "st-256color"
		 "st52-color"
		 "stterm"
		 "stterm-16color"
		 "stterm-256color"
		 "sun-color"
		 "tek4205"
		 "teken"
		 "teraterm"
		 "teraterm2.3"
		 "teraterm4.59"
		 "terminator"
		 "terminology"
		 "ti928"
		 "ti928-8"
		 "ti_ansi"
		 "tmux"
		 "tmux-256color"
		 "tt52"
		 "tw100"
		 "tw52"
		 "tw52-color"
		 "uwin"
		 "vt100+"
		 "vte"
		 "vte-2007"
		 "vte-2008"
		 "vte-2012"
		 "vte-2014"
		 "vte-256color"
		 "vtnt"
		 "vt-utf8"
		 "vv100"
		 "vwmterm"
		 "wsvt25"
		 "wsvt25m"
		 "wy350"
		 "wy350-vb"
		 "wy350-w"
		 "wy350-wvb"
		 "wy370"
		 "wy370-101k"
		 "wy370-105k"
		 "wy370-EPC"
		 "wy370-nk"
		 "wy370-rv"
		 "wy370-vb"
		 "wy370-w"
		 "wy370-wvb"
		 "wyse350"
		 "wyse350-vb"
		 "wyse350-w"
		 "wyse350-wvb"
		 "wyse370"
		 "xfce"
		 "xiterm"
		 "xnuppc"
		 "xnuppc-100x37"
		 "xnuppc-112x37"
		 "xnuppc-128x40"
		 "xnuppc-128x48"
		 "xnuppc-144x48"
		 "xnuppc-160x64"
		 "xnuppc-200x64"
		 "xnuppc-200x75"
		 "xnuppc-256x96"
		 "xnuppc-80x25"
		 "xnuppc-80x30"
		 "xnuppc-90x30"
		 "xnuppc-b"
		 "xnuppc+c"
		 "xnuppc-f"
		 "xnuppc-f2"
		 "xterm"
		 "xterm1"
		 "xterm-1002"
		 "xterm-1003"
		 "xterm-1005"
		 "xterm-1006"
		 "xterm-16color"
		 "xterm-256color"
		 "xterm+256color"
		 "xterm+256setaf"
		 "xterm-88color"
		 "xterm+88color"
		 "xterm-8bit"
		 "xterm-basic"
		 "xtermc"
		 "xterm-color"
		 "xterm-hp"
		 "xterm-new"
		 "xterm-nic"
		 "xterm-noapp"
		 "xterm-sco"
		 "xterms-sun"
		 "xterm-sun"
		 "xterm-utf8"
		 "xterm-vt220"
		 "xterm-x10mouse"
		 "xterm-x11hilite"
		 "xterm-x11mouse"
		 "xterm-xf86-v32"
		 "xterm-xf86-v33"
		 "xterm-xf86-v333"
		 "xterm-xf86-v40"
		 "xterm-xf86-v43"
		 "xterm-xf86-v44"
		 "xterm-xfree86"
		 "xterm-xi"
		 "xwsh"
		 ))))

;; 2542: g_log_writer_default
(define (log-writer-default log-level fields-alist)
  (unless (null? fields-alist)
    
    ;; If the log-level is not in the DEFAULT_LEVELS, it is only
    ;; logged if there is a DOMAIN key in the fields which matches a
    ;; string in the MESSAGES_DEBUG environment variable.
    (let ((domains (or (getenv "MESSAGES_DEBUG") "")))
      (when (or (logtest log-level DEFAULT_LEVELS)
		(string-contains domains "all")
		(false-if-exception
		 (string-contains domains
				  (assoc-ref fields-alist "DOMAIN"))))
	;; We try to send to the journald first, or then fallback to the
	;; standard streams.
	(if (and (not (and (log-writer-is-journald? (fileno (current-error-port)))
			   (log-writer-journal log-level fields-alist)))
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
	      #t))))))

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

;; 2683: g_return_if_fail_warning
(define (if-false-warning log-domain pretty-function expression)
  (%log log-domain LOG_LEVEL_CRITICAL
	"~a: assertion '~a' failed"
	pretty-function
	expression))

;; 2850: _g_log_fallback_handler
(define (%log-fallback-handler log-domain log-level _message)
  "A very basic logging handler."
  (let ((prefix (mklevel-prefix log-level #f))
	(message
	 (if (or (not _message) (not (string? _message))
		 (string-null? _message))
	     "(NULL) message"
	     _message))
	(port (log-level->port log-level)))
    (if log-domain
	(write-string port "\n")
	(write-string port "\n** "))
    (write-string port "(process:")
    (write-string port (number->string (getpid)))
    (write-string port "):")
    (when log-domain
      (write-string port log-domain)
      (write-string port "-"))
    (write-string port prefix)
    (write-string port ": ")
    (write-string port message)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (logging-init)
  (set! *log-writer-func* log-writer-default)
  (set! *default-log-func* log-default-handler)

  ;; glib-init.c:85 g_log_msg_prefix
  (set! *log-msg-prefix* (logior
			  LOG_LEVEL_ERROR
			  LOG_LEVEL_WARNING
			  LOG_LEVEL_CRITICAL
			  LOG_LEVEL_DEBUG))
  
  ;; glib-init.c:86 g_log_always_fatal
  (set! *log-always-fatal* LOG_FATAL_MASK)
  (set! *log-start-time* (monotonic-time)))

(logging-init)
