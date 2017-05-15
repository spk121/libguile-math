(define-module (mlg logging)
  #:export())

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


(define LOG_FLAG_RECURSION (ash 1 0))
(define LOG_FLAG_FATAL     (ash 1 1))
(define LOG_LEVEL_ERROR    (ash 1 2))
(define LOG_LEVEL_CRITICAL (ash 1 3))
(define LOG_LEVEL_WARNING  (ash 1 4))
(define LOG_LEVEL_MESSAGE  (ash 1 5))
(define LOG_LEVEL_INFO     (ash 1 6))
(define LOG_LEVEL_DEBUG    (ash 1 7))
(define LOG_LEVEL_MASK     (lognot-uint16 (logior LOG_FLAG_RECURSION
						  LOG_FLAG_FATAL)))
(define LOG_FATAL_MASK    (logior LOG_FLAG_RECURSION LOG_LEVEL_ERROR))

(define ALERT_LEVELS      (logior LOG_LEVEL_ERROR
				  LOG_LEVEL_CRITICAL
				  LOG_LEVEL_WARNING))
(define DEFAULT_LEVELS    (logior LOG_LEVEL_ERROR
				  LOG_LEVEL_CRITICAL
				  LOG_LEVEL_WARNING
				  LOG_LEVEL_MESSAGE))
(define INFO_LEVELS       (logior LOG_LEVEL_INFO
				  LOG_LEVEL_DEBUG))


(define *messages-lock* (make-mutex))
(define *log-structured-depth*  (make-fluid 0))
(define *log-writer-func*  log-writer-default)

;; 523: aka _g_log_abort
(define (%log-abort breakpoint)
  (if breakpoint
      (trap-here-0)
      (exit 1)))

(define (log-level->port log-level)
  (if (logtest log-level
	       (logior LOG_LEVEL_ERROR
		       LOG_LEVEL_WARNING
		       LOG_LEVEL_CRITICAL
		       LOG_LEVEL_MESSAGE))
      (current-error-port)
      (current-output-port)))

;; aka g_log_structured_array
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

;; aka g_log_writer_default
(define (log-writer-default log-level fields-alist)
  (unless (null? fields-alist)
    (cond
     ((not (logtest log-level DEFAULT_LEVELS))
      (let ((domains (getenv "MESSAGES_DEBUG")))
	(cond
	 ((or (logtest log-level INFO_LEVELS)
	      (not domains))
	  #f)
	 ((and (not (equal? domains "all"))
	       (not (equal? domain (assoc-ref fields-alist "DOMAIN"))))
	  #f
	 

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

