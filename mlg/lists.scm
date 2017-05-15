(define-module (mlg lists))


FIXME:BROKEN!!

;; and-let-until-fail* proc (c ...) . body

;; Like and-let*, but, applies the predicate PROC across the let*
;; arguments arguments list, returning the first
;; element in the list where the application of the predicate
;; returns false.  If the application of the predicate succeeds
;; on every element in the list, the body is run.

(define-syntax %and-let-until-fail
  (lambda (form)
    (syntax-case form ()
      
      ;; Handle zero-clauses special-case.
      ((_ orig-form proc () . body)
       #'(begin #t . body))
      
      ;; Reduce clauses down to one regardless of body.
      ((_ orig-form proc ((var expr) rest . rest*) . body)
       (identifier? #'var)
       #'(let ((var expr))
           (if (not (proc var))
	       var
	       (%and-let-until-fail* orig-form proc (rest . rest*) . body))))
      ((_ orig-form proc ((expr) rest . rest*) . body)
       #'((lambda (val)
	    (if (proc val)
		val
		(%and-let-until-fail* orig-form proc (rest . rest*) . body)))
	  expr))
      ((_ orig-form proc (var rest . rest*) . body)
       (identifier? #'var)
       #'(if (not (proc var))
	     var
	     (%and-let-until-fail* orig-form proc (rest . rest*) . body)))
      
      ;; Handle 1-clause cases without a body.
      ((_ orig-form proc ((var expr)))
       (identifier? #'var)
       #'(let ((var expr))
	   (if (not (proc var))
	       var
	       #t)))
      ((_ orig-form proc ((expr)))
       #'((lambda (var)
	    (if (not (proc var))
		var
		#t))
	  expr))
      ((_ orig-form proc (var))
       (identifier? #'var)
       #'(if (not (proc var))
	     var
	     #t))
      
      ;; Handle 1-clause cases with a body.
      ((_ orig-form proc ((var expr)) . body)
       (identifier? #'var)
       #'(let ((var expr))
	   (if (not (proc var))
	       var
	       (begin . body))))
      ((_ orig-form proc ((expr)) . body)
       #'((lambda (var)
	    (if (not (proc var))
		var
		(begin . body)))
	  expr))
      ((_ orig-form proc (var) . body)
       (identifier? #'var)
       #'(if (not (proc var))
	     var
	     (begin . body)))
      
      ;; Handle bad clauses.
      ((_ orig-form proc (bad-clause . rest) . body)
       (syntax-violation 'and-let-until-fail* "Bad clause" #'orig-form #'bad-clause))))
  )

(define-syntax and-let-until-fail*
  (lambda (form)
    (syntax-case form ()
      ((_ proc (c ...) body ...)
       #`(%and-let-until-fail* #,form proc (c ...) body ...)))))
