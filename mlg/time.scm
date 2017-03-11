(define-module (mlg time)
  #:use-module (srfi srfi-9)
  #:export (monotonic-time
	    timer?
	    timer-new
	    timer-restart
	    timer-stop
	    timer-continue
	    timer-elapsed
	    ))

(define (monotonic-time)
  "Return the current time in microseconds since 1970."  
  (let ((now (gettimeofday)))
    (+ (* 1000000 (car now))
       (cdr now))))

(define-record-type <timer>
  (make-timer start-time end-time running?)
  timer?
  (start-time  timer-start-time set-timer-start-time!)
  (end-time    timer-end-time   set-timer-end-time!)
  (running?    timer-running?   set-timer-running!))

(define (timer-new)
  "Creates a new timer and starts timing."
  (make-timer (monotonic-time)
	      0
	      #t))

(define (timer-restart timer)
  "Restarts the timer, with the current time as the start time."
  ;; (assert-type timer? timer)
  (set-timer-running! timer #t)
  (set-timer-start-time! timer (monotonic-time)))

(define (timer-elapsed timer)
  "Returns the elapsed time in microseconds for this timer."
  ;; (assert-type timer? timer)
  (when (timer-running? timer)
    (set-timer-end-time! timer (monotonic-time)))

  (- (timer-end-time timer)
     (timer-start-time timer)))
    
(define (timer-stop timer)
  "Stops the timer. Also returns the elapsed time in microseconds for
this timer."
  ;; (assert-type timer? timer)
  (set-timer-running! timer #f)
  (set-timer-end-time! timer (monotonic-time))
  (timer-elapsed timer))

(define (timer-continue timer)
  "Resumes a timer that has been stopped with 'timer-stop'."
  ;; (assert-type timer? timer)
  (let ((elapsed (- (timer-end-time timer)
		    (timer-start-time timer))))
    (set-timer-start-time! timer (- (monotonic-time)
				    elapsed))
    (set-timer-running! timer #t)))

