(define-module (mlg ed bookmark)
  #:export (set-bookmark-cbuffer
	    get-bookmark-cbuffer
	    get-bookmark-cb))

(define *bookmark-cbuffer* #f)

(define (set-bookmark-cbuffer cbuf)
  "Set the cbuffer to be used when looking up bookmarks."
  (set! *bookmark-cbuffer* cbuf))

(define (get-bookmark-cbuffer)
  "Return the current setting of the cbuffer to be used when looking
up bookmarks."
  *bookmark-cbuffer*)

(define (get-bookmark-cb c)
  "Given a single letter C, look up a bookmark by that
name in the current buffer, which must have been
set using set-bookmark-buffer!"
  (ed-get-mark *bookmark-cbuffer* c))
