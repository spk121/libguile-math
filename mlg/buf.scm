;; Part of the ed parser
(define-module (mlg buf)
  #:use-module (mlg utils)
  #:export (init-buffers))

(define newline-added #f)

(define sfn "")				; scratch file name
(define sfp #f)				; scratch file port

(define (open-sbuf)
  "Open scratch file"
  (set! newline-added #f)
  (set! sfn (build-filename (get-tmp-dir) "ed.XXXXXX"))
  (set! sfp (mkstemp! sfn)))

(define (init-buffers)
  "Open scratch buffer; initialize line queue."
  (setvbuf (current-input-port) _IONBF)
  (open-sbuf))
