(define-module (mlg ed bmark)
  #:use-module (gano CBuffer)
  #:export (bmark-set-default-cbuf
	    bmark-default-cb
	    bmark-set))

(define *bmark-cbuf-cur* #f)

(define (bmark-set-default-cbuf cbuf)
  (set! *bmark-cbuf-cur* cbuf))

(define (bmark-default-cb id)
  (if (cbuffer? *bmark-cbuf-cur*)
      (ed-get-mark cbuf id)
      #f))

(define (bmark-set lineno name)
  (if (cbuffer? *bmark-cbuf-cur*)
      (ed-mark *bmark-cbuf-cur* lineno name)
      #f))
