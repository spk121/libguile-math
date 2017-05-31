;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg ed repl) - an Ed-like read-eval-print loop
;;; Copyright (C) 2017 Michael L. Gran <spk121@yahoo.com>
;;;
;;; This program is free software: you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundataion, either version 3 of
;;; this License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see
;;; <http://www.gnu.org/licenses/>

(define-module (mlg ed repl)
  #:use-module (gano CBuffer)
  #:use-module (gano poslist)
  #:use-module (ice-9 rdelim)
  #:use-module (mlg ed address)
  #:use-module (mlg ed bmark)
  #:use-module (mlg ed regex)
  #:use-module (mlg logging)
  #:use-module (mlg port)
  #:use-module (mlg strings)
  #:use-module (mlg utils)
  #:use-module (mlg typechecking)
  #:use-module (oop goops)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  )

;; Globals
(define OK 0)
(define EOF -1)
(define ERR -2)
(define EMOD -3)
(define FATAL -4)

(define (dispatch:key x)        (list-ref x 0))
(define (dispatch:addr-count x) (list-ref x 1))
(define (dispatch:addr-start x) (list-ref x 2))
(define (dispatch:addr-end x)   (list-ref x 3))
(define (dispatch:zero-addr-ok? x) (list-ref x 4))
(define (dispatch:parser x)     (list-ref x 5))
(define (dispatch:suffix? x)    (list-ref x 6))
(define (dispatch:append? x)    (list-ref x 7))
(define (dispatch:op x)         (list-ref x 8))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; An <EdRepl> is a read-eval-print loop for a CBuffer.

(define-class <EdRepl> (<CBuffer>)

  ;; The user's home directory
  (m_Home #:init-thunk get-home-dir
          #:getter get-home
          #:setter set-home!)

  ;; If #t, in the middle of a global command.
  (m_IsGlobal #:init-value #f)

  ;; If #t, buffer has been modified since the last save.
  (m_Modified #:init-value #f)

  ;; If #t, we are interactive
  (m_Interactive #:init-value #t)

  (m_ErrMsg #:init-value ""
            #:getter get-err-msg
            #:setter set-err-msg!)
  (m_Status #:init-value 0
            #:getter get-status
            #:setter set-status!)

  ;; The following parameter describe the verbosity of the REPL

  ;; If true, suppress almost all output
  (m_Scripted #:init-value #f)

  ;; If true, print full error messages instead of the
  ;; abbreviated error message, which is just '?'
  (m_Garrulous #:init-value #f
               #:getter get-garrulous
               #:setter set-garrulous!)

  ;; If true, we are printing debug information.
  (m_Verbose #:init-value #t
             #:getter get-verbose)

  ;; If true, we are printing a prompt when prompting for a command.
  (m_PromptActive #:init-value #f
                  #:getter get-prompt-active
                  #:setter set-prompt-active!)

  ;; When m_PromptActive is #t, this string is used as a prompt.
  (m_PromptString #:init-value "command> "
                   #:getter get-prompt-string
                   #:setter set-prompt-string!)

  (m_DispatchTable #:init-value '()
                   ;; 1. Shortcut character
                   ;; 2. Required number of addresses
                   ;; 3,4. The default addresses
                   ;; 5. If zero is a valid address
                   ;; 5. How to parse additional info
                   ;; 6. Does command accept standard suffix
                   ;; 7. Does command accept input lines
                   ;; 8. Operation function
                   #:getter get-dispatch-table
                   #:setter set-dispatch-table!)

  )

(define (make-ed-repl)
  (let ((ER (make <EdRepl>)))
    ER))

(define (ed-repl? buf)
  (is-a? buf <EdRepl>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods

(define (convert-default-addr sym line-cur line-last)
  (cond
   ((integer? sym)
    sym)
   ((eqv? 'dot)
    line-cur)
   ((eqv? '$)
    line-last)
   ((eqv? 'dot+1)
    (1+ line-cur))))

(define-method (ed-repl-construct-bookmark-callback (repl <EdRepl>))
  (lambda (name)
    (log-debug "In bookmark callback ~a ~a" repl name)
    ;; CBuffer bookmarks are zero-indexed. Ed bookmarks are 1-indexed.
    (let ((pos (bookmark-get (get-bookmarks repl) name)))
      (log-debug-locals)
      (if pos
	  (1+ (car pos))
	  #f))))

(define-method (ed-repl-display-lines (repl <EdRepl>) from to suffix)
  "Print a range of lines to stdout, using Ed coordinates, where both
FROM and TO are 1-indexed and inclusive."
  (do ((i from (1+ i)))
      ((>= i to))
    (let ((str (get-text-line repl (1- i))))
      (when (member #\n (string->list suffix))
        (format #t "~d~/" i))
      (if (member #\l (string->list suffix))
          (format #t "~a$~%" (string->ed-escaped-string str))
          (format #t "~a~%" str)))))

(define (put-tty-line str n suffix)
  "Print text to stdout."
  (warn-if-false (string? str))
  (warn-if-false (integer? n))
  (warn-if-false (string? suffix))

  (if (member #\n (string->list suffix))
      (format #t "~d~/" n))
  (if (member #\l (string->list suffix))
      (format #t "~a$~%" (string->ed-escaped-string str))
      (format #t "~a~%" str)))

(define-method (ed-repl-get-line-cur-in-ed-coordinates (repl <EdRepl>))
  "Gets the active data line using Ed's 1-indexed coordinate system."
  (if (zero? (get-line-count repl))
      0
      (1+ (get-line-cur repl))))

(define-method (ed-repl-get-line-last-in-ed-coordinates (repl <EdRepl>))
  "Gets the last data line using Ed's 1-indexed coordinate system."
  (get-line-count repl))

(define-method (ed-repl-do (repl <EdRepl>))
  (when (get-verbose repl)
    (%dump-cbuffer repl))
  (when (get-prompt-active repl)
    (display (get-prompt-string repl))
    (force-output))

  (set-status! repl OK)

  ;; Read in one line of text from the user.
  (let ((port (open-input-string (read-line (current-input-port)))))

    (and-let* ((addr-range (ed-repl-parse-address-range repl port))
               (c (read-char-safe port))
               (test1 (ed-repl-validate-command-key repl c))
               (test2 (ed-repl-validate-addr-range-for-key repl c addr-range)))
      ;; So far, so good.  The character C maps to an operation and we
      ;; have the correct number of addresses for that operation.
      (and-let* ((op (assoc c (get-dispatch-table repl)))

                 (addr (ed-repl-unpack-addr-range repl op addr-range))
                 (extra
                  ;; Maybe there are other things that need to be parsed,
                  ;; depending on the command.
                  (case (dispatch:parser op)
                    ((address)
                     (ed-repl-parse/validate-3rd-address repl port))
		    ((bookmark)
		     (ed-repl-parse/validate-bookmark-name repl port))
                    ((file)
                     (ed-repl-parse/validate-filename repl port))
                    ((regex)
                     (ed-repl-parse-regex))
                    ((regex+cmd)
                     (ed-repl-parse-regex+cmd))
                    ((regex+replace)
                     (ed-repl-parse-regex+replace))
                    ((shell)
                     (ed-repl-parse-shell))
                    (else
                     #t)))
                 (suffix
                  ;; Some commands take the characters 'l', 'n', or 'p'
                  ;; as a suffix
                  (if (dispatch:suffix? op)
                      (ed-repl-parse/validate-suffix repl port)
                      ""))
                 (txt
                  ;; Some commands let the user type some lines terminated
                  ;; by a dot
                  (if (dispatch:append? op)
                      (ed-repl-parse/validate-input-lines repl port)
                      '())))
        ;; So, all the parsing is done, and we can finally
        ;; run a command.
        ;; (log-debug-locals)
        ((dispatch:op op) repl addr extra suffix txt)))
    (if (< (get-status repl) 0)
        (if (get-garrulous repl)
            (begin
              (display (get-err-msg repl))
              (newline))
            ;; else
            (begin
              (display "?")
              (newline))))))

(define-method (ed-repl-parse-address-range (repl <EdRepl>) port)
  (let ((addr-range (addr-get-range port
                                    (ed-repl-get-line-cur-in-ed-coordinates repl)
                                    (ed-repl-get-line-last-in-ed-coordinates repl)
				    (ed-repl-construct-bookmark-callback repl)
                                    regex-default-cb)))
    (unless addr-range
      (set-err-msg! repl (addr-get-range-error))
      (set-status! repl ERR))
    addr-range))

(define-method (ed-repl-parse/validate-bookmark-name (repl <EdRepl>) port)
  (let ((name (read-char-safe port)))
    (cond
     ((not (char-lower-case? name))
      (set-err-msg! repl
		    (format #f "invalid bookmark name '~a'" name))
      #f)
     (else
      name))))

(define-method (ed-repl-parse/validate-3rd-address (repl <EdRepl>) port)
  (let ((addr3 (ed-repl-parse-address-range repl port)))
    (cond
     ((< (length addr3) 1)
      (set-err-msg! repl
                    (format #f "invalid 3rd address"))
      (set-status! repl ERR)
      #f)
     (else
      (last addr3)))))

(define-method (ed-repl-parse/validate-filename (repl <EdRepl>) port)
  (let ((fname (read-ed-filename port)))
    (if (not fname)
        (begin
          (set-err-msg! repl (get-read-ed-filename-err))
          (set-status! repl ERR)
          #f)
        ;; else
        fname)))

(define-method (ed-repl-parse/validate-input-lines (repl <EdRepl>) port)
  "Some commands allow the entry of text lines, ended by entering
a single '.' on its own line"
  ;; (drain-input port)
  (let loop ((out '())
             (line (read-line (current-input-port))))
    (cond
     ((eof-object? line)
      out)
     ((string=? line ".")
      out)
     (else
      (loop (append out (list line))
            (read-line (current-input-port)))))))

(define-method (ed-repl-parse/validate-suffix (repl <EdRepl>) port)
  "Some commands allow 'l' 'n' or 'p' afterward to print a result.
Returns a string containing zero or more of 'l', 'n', and 'p'."
  (let loop ((ret (peek-char-safe port))
             (out ""))
    (if (member ret (string->list "lnp"))
        (begin
          (read-char port)
          (loop (peek-char-safe port) (string-append out (string ret))))
        ;; else
        out)))

;; Arguments
;;
;; If an operation takes zero addresses but receives more than
;; zero addresses, it is an error.

;; If an operation takes more than zero addresses but receives
;; zero addresses, the defaults are used.

;; If an operation takes two addresses but receives one
;; address, apparently, the given address is used for both.
(define-method (ed-repl-unpack-addr-range (repl <EdRepl>) op addr-list)
  (let ((addr-count-required (dispatch:addr-count op))
        (addr-list-len (length addr-list))
        (line-cur (ed-repl-get-line-cur-in-ed-coordinates repl))
        (line-last (ed-repl-get-line-cur-in-ed-coordinates repl)))

    (cond
     ((and (= addr-count-required 0) (> addr-list-len 0))
      (set-err-msg! repl
        (format #f "command ~s expects zero addresses" (dispatch:key op)))
      #f)
     (else
      (let ((addr
             (cond
              ((= addr-count-required 0)
               '())
              ((= addr-count-required 1)
               (cond
                ((>= addr-list-len 1)
                 (list (last addr-list)))
                (else
                 (list (convert-default-addr (dispatch:addr-start op) line-cur line-last)))))
              ((= addr-count-required 2)
               (cond
                ((= addr-list-len 2)
                 addr-list)
                ((= addr-list-len 1)
                 (list (last addr-list)
                       (last addr-list)))
                ((= addr-list-len 0)
                 (list (convert-default-addr (dispatch:addr-start op) line-cur line-last)
                       (convert-default-addr (dispatch:addr-end op) line-cur line-last)))))
              (else
               ;; Should never reach here.
               (set-err-msg! repl
                 (format #f "command ~a expects ~a addresses but received ~a addresses"
                         (dispatch:key op) addr-count-required addr-list-len))
               #f))))
        addr)))))

(define-method (ed-repl-validate-addr-range-for-key (repl <EdRepl>) c addr-range)
  "Return #t if addr-range is valid for the command described by the key
C."
  (let* ((op (assoc c (get-dispatch-table repl)))
         (required-count (dispatch:addr-count op)))
    (cond
     ((and (zero? required-count)
           (not (zero? (length addr-range))))
      (set-err-msg! repl
                    (format #f "command '~a' takes zero arguments" c))
      (set-status! repl ERR)
      #f)

     ((and (not (dispatch:zero-addr-ok? op))
           (any zero? addr-range))
      (set-err-msg! repl
                    (format #f "address out of range: ~a" addr-range))
      #f)

     (else
      #t))))

(define-method (ed-repl-validate-command-key (repl <EdRepl>) c)
  "Returns #t if C is a known 1-letter Ed command, else returns #f."
  (if (not (assoc c (get-dispatch-table repl)))
      (begin
        (set-err-msg! repl (format #f "unknown command ~a" c))
        (set-status! repl ERR)
        #f)
      #t))

(define-method (op-append (repl <EdRepl>) addr extra suffix txt)
  "Appends lines after the given address."

  ;; The CBuffer primitive ed-append inserts expects the line number
  ;; where the insertions happens, and its line numbers are
  ;; zero-indexed.

  ;; Ed line numbers are 1-indexed and indicate the line after which
  ;; the insertion occurs. Zero means before the first line.

  ;; So in this case Ed address == CBuffer address
  (ed-append repl (first addr) txt)

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-change (repl <EdRepl>) addr special suffix append)
  "Appends lines after the given address."
  (warn-if-false (list-length-2? addr))
  (warn-if-false (list-of-integers? addr))
  (warn-if-false (list-of-strings? append))
  (warn-if-false (string? suffix))
  ;; (log-debug-locals)

  ;; Move the current position out of the way, for the moment.
  (set-line-cur! repl 0)

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive).

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). As an special case, zero is
  ;; mapped to one.
  (let ((start (1- (max 1 (first addr))))
        (end (max 1 (second addr))))
    (ed-change repl start end append))

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-delete (repl <EdRepl>) addr special suffix append)
  "Deletes the addressed lines from the buffer."
  ;; Move the current position out of the way, for the moment.
  (set-line-cur! repl 0)

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive).

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). An address of zero is invalid.
  (ed-delete repl (1- (first addr)) (second addr))

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-help (repl <EdRepl>) addr special suffix append)
  "Display the last error message."
  (display (get-err-msg repl))
  (newline))

(define-method (op-help-mode (repl <EdRepl>) addr special suffix append)
  "Toggle verbose display of error messages."
  (set-garrulous! repl (not (get-garrulous repl))))

(define-method (op-insert (repl <EdRepl>) addr special suffix txt)
  "Appends lines before the given address."

  ;; The CBuffer primitive ed-append inserts expects the line number
  ;; where the insertions happens, and its line numbers are
  ;; zero-indexed.

  ;; Ed line numbers are 1-indexed and indicate the line after which
  ;; the insertion occurs. Zero means before the first line.

  ;; So in this case Ed address = CBuffer address - 1
  ;; The spec says to treat zero as one.
  ;; FIXME: add strict address checking
  (ed-append repl (max 0 (1- (first addr))) txt)

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-join (repl <EdRepl>)  addr special suffix append)
  "Joins contiguous lines."
  ;; The CBuffer primitive ed-join inserts expects the a start
  ;; address (inclusive) and end address (exclusive) where the
  ;; join happens, and its line numbers are zero-indexed.

  ;; Ed line numbers are 1-indexed and are start (inclusive)
  ;; and end (inclusive). Zero is invalid.
  (ed-join repl (1- (first addr)) (second addr))

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-list (repl <EdRepl>) addr special suffix append)
  "Display the addressed lines."
  (ed-repl-display-lines repl (first addr) (1+ (second addr)) "l")
  0)

(define-method (op-move (repl <EdRepl>) addr addr3 suffix append)
  "Moves the addressed lines after the line addressed by the third address.
If the 3rd address is zero, it moves the addressed lines to the beginning."
  ;; Move the current position out of the way, for the moment.
  (set-line-cur! repl 0)

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive), and will
  ;; move to the zero-indexed 3rd address.

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). The 3rd address is the
  ;; 1-indexed line after which to move the lines.  Zero indicates
  ;; that the lines are to be inserted before the 1st line.
  (ed-move repl (1- (first addr)) (second addr) addr3)

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-number (repl <EdRepl>) addr special suffix append)
  (ed-repl-display-lines repl (first addr) (1+ (second addr)) "n")
  0)

(define-method (op-print (repl <EdRepl>) addr special suffix append)
  (ed-repl-display-lines repl (first addr) (1+ (second addr)) "p")
  0)

(define-method (op-prompt (repl <EdRepl>) addr special suffix append)
  (set-prompt-active! repl (not (get-prompt-active repl)))
  0)

(define-method (op-copy (repl <EdRepl>) addr addr3 suffix append)
  "Copies the addressed lines after the line addressed by the third address.
If the 3rd address is zero, it copies the addressed lines to the
beginning."
  (log-debug-locals)
  ;; Move the current position out of the way, for the moment.
  (set-line-cur! repl 0)

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive), and will
  ;; copy to the zero-indexed 3rd address.

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). The 3rd address is the
  ;; 1-indexed line after which to move the lines.  Zero indicates
  ;; that the lines are to be inserted before the 1st line.
  (ed-copy repl (1- (first addr)) (second addr) addr3)

  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) suffix))
  0)

(define-method (op-line-number (repl <EdRepl>) addr special suffix append)
  "Print the addressed line."
  (display (last addr))
  (newline)
  0)

(define-method (op-mark (repl <EdRepl>) addr name suffix append)
  ;; Ed bookmarks are 1-indexed.  CBuffer bookmarks are zero-indexed.
  (ed-mark repl (1- (last addr)) name)
  (unless (string-null? suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl))
			   suffix))
  0)

(define-method (op-null (repl <EdRepl>) addr addr3 suffix append)
  "Print the addressed line."
  (set-line-cur! repl (last addr))
  (ed-repl-display-lines repl (get-line-cur repl) (1+ (get-line-cur repl)) "p")
  0)

(define rpl (make-ed-repl))
(set-dispatch-table! rpl
                   `((#\a    1 dot   #f    #t null          #t #t  ,op-append)
                     (#\c    2 dot   dot   #t null          #t #t  ,op-change)
                     (#\d    2 dot   dot   #f null          #t #f  ,op-delete)
                     (#\e    0 #f    #f    #f file          #f #f  op-edit)
                     (#\E    0 #f    #f    #f file          #f #f  op-edit-without-checking)
                     (#\f    0 #f    #f    #f file          #f #f  op-filename)
                     (#\g    2 1     $     #f regex+cmd     #t #f  op-global)
                     (#\G    2 1     $     #f regex         #t #f  op-global-interactive)
                     (#\h    0 #f    #f    #f null          #t #f  ,op-help)
                     (#\H    0 #f    #f    #f null          #t #f  ,op-help-mode)
                     (#\i    1 dot   #f    #t null          #t #t  ,op-insert)
                     (#\j    2 dot   dot+1 #f null          #t #f  ,op-join)
                     (#\k    1 dot   #f    #f bookmark      #t #f  ,op-mark)
                     (#\l    2 dot   dot   #f null          #t #f  ,op-list)
                     (#\m    2 dot   dot   #t address       #t #f  ,op-move)
                     (#\n    2 dot   dot   #f null          #t #f  ,op-number)
                     (#\p    2 dot   dot   #f null          #t #f  ,op-print)
                     (#\P    0 #f    #f    #f null          #t #f  ,op-prompt)
                     (#\q    0 #f    #f    #f null          #f #f  op-quit)
                     (#\Q    0 #f    #f    #f null          #f #f  op-quit-without-checking)
                     (#\r    1 $     #f    #t file          #f #f  op-read)
                     (#\s    2 dot   dot   #f regex+replace #t #f  op-substitute)
                     (#\t    2 dot   dot   #t address       #t #f  ,op-copy)
                     (#\u    0 #f    #f    #f null          #t #f  op-undo)
                     (#\v    2 1     $     #f regex+cmd     #t #f  op-global-non-matched)
                     (#\V    2 1     $     #f regex         #t #f  op-global-interactive-non-matched)
                     (#\w    2 1     $     #f file          #f #f  op-write)
                     (#\=    1 $     #f    #f null          #t #f  ,op-line-number)
                     (#\!    0 #f    #f    #f shell         #f #f  op-shell-escape)
                     (#\nul  1 dot+1 #f    #f null          #t #f  ,op-null)))

(while #t
  (ed-repl-do rpl))
