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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (mlg ed address)
  ;; #:use-module (mlg ed bmark)
  #:use-module (mlg ed filename)
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
(define QUIT -5)

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
  (m_Modified #:init-value #f
              #:getter get-modified
              #:setter set-modified!)

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
  (m_Scripted #:init-value #f
              #:getter get-scripted)

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

  (m_Filename #:init-value ""
              #:getter get-filename
              #:setter set-filename!)

  (m_LastCommand #:init-value #\null
                 #:getter get-last-command
                 #:setter set-last-command!)

  (m_LastReplacementString #:init-value #f
                           #:getter get-last-replacement-string
                           #:setter set-last-replacement-string!)
  )

(define (make-ed-repl)
  (let ((ER (make <EdRepl>)))
    ER))

(define (ed-repl? buf)
  (is-a? buf <EdRepl>))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods

(define (convert-default-addr Sym LCur LLast)
  (warn-if-false (integer-nonnegative? LCur))
  (warn-if-false (integer-nonnegative? LLast))

  (cond
   ((integer? Sym)
    Sym)
   ((eqv? 'dot Sym)
    LCur)
   ((eqv? '$ Sym)
    LLast)
   ((eqv? 'dot+1 Sym)
    (1+ LCur))))

(define-method (ed-repl-construct-bookmark-callback (repl <EdRepl>))
  (lambda (name)
    ;; CBuffer bookmarks are zero-indexed. Ed bookmarks are 1-indexed.
    (let ((pos (get-bookmark-line repl name)))
      (if pos
          (1+ (car pos))
          #f))))

(define-method (ed-repl-display-lines (Repl <EdRepl>) LStart LEnd Suffix)
  "Print a range of lines to stdout from LStart (1-indexed, inclusive)
to LEnd (1-indexed, inclusive), using the format described in Suffix"
  (warn-if-false (integer-nonnegative? LStart))
  (warn-if-false (integer-nonnegative? LEnd))
  (warn-if-false (<= LStart LEnd))
  (warn-if-false (string? Suffix))

  (do ((i LStart (1+ i)))
      ((> i LEnd))
    (let ((str (get-text-line Repl (1- i))))
      (when (member #\n (string->list Suffix))
        (format #t "~d~/" i))
      (if (member #\l (string->list Suffix))
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

;(define-method (ed-repl-do (repl <EdRepl>) PortOrStr)
(define (ed-repl-do repl PortOrStr)
  (warn-if-false (cbuffer? repl))

  (when (get-verbose repl)
    (%dump-cbuffer repl))
  (when (get-prompt-active repl)
    (display (get-prompt-string repl))
    (force-output))

  (set-status! repl OK)

  ;; Read in one line of text from the user.
  (let ((port (open-input-string (if (string? PortOrStr)
                                     PortOrStr
                                     (read-line (current-input-port))))))

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
                     (ed-repl-parse-regex repl port))
                    ((regex+cmd)
                     (ed-repl-parse-regex+cmd repl port))
                    ((regex+replace)
                     (ed-repl-parse-regex+replace repl port))
                    ((shell)
                     (ed-repl-parse-shell repl port))
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
        ((dispatch:op op) repl addr extra suffix txt)
        (set-last-command! repl c)))

    (if (member (get-status repl) (list ERR))
        (if (get-garrulous repl)
            (begin
              (display (get-err-msg repl))
              (newline))
            ;; else
            (begin
              (display "?")
              (newline))))
    (if (= QUIT (get-status repl))
        #f
        #t)))

(define-method (ed-repl-parse-address-range (repl <EdRepl>) port)
  (let ((addr-range (addr-get-range port
                                    (ed-repl-get-line-cur-in-ed-coordinates repl)
                                    (ed-repl-get-line-last-in-ed-coordinates repl)
                                    (ed-repl-construct-bookmark-callback repl)
                                    regex-default-cb)))
    ;; (log-debug-locals)
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
      (string name)))))

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
  "Extracts a filename from the given port.  If there is not text to
be read in the port, it returns the previously used filename.  If the
first character in the filename is '!', it reads a shell command
instead.  It will return the string, or #f on failure."
  (let ((txt ""))
    (if (not (eof-object? (peek-char port)))
        (set! txt (read-whitespace port)))
    (if (eof-object? (peek-char port))
        (if (string-null? (get-filename repl))
            (begin
              (set-err-msg! repl "missing filename")
              (set-status! repl ERR)
              (unread-string txt port)
              #f)
            ;; else
            (get-filename repl))
        ;; else
        (let ((str (string-trim-both (read-line port))))
          (if (not (string-starts-with? str #\!))
              (set-filename! repl str))
          str))))

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
  (warn-if-false (input-port? port))
  (read-filter "lnp" port))

(define-method (ed-repl-parse-regex (repl <EdRepl>) port)
  "Returns the regex string, which
begins and ends with the delimeter characters (usually '/')."
  (write (current-source-location))
  (read-regex-string port))

(define-method (ed-repl-parse-regex+cmd (repl <EdRepl>) port)
  "Returns a pair.  The CAR is the regex string, which
begins and ends with the delimeter characters (usually '/').  The CDR
is the remainder of the string."
  (cons (read-regex-string port)
        (read-string port)))

(define-method (ed-repl-parse-regex+replace (repl <EdRepl>) port)
  "Returns a list.  The first element the regex string, which
begins and ends with the delimeter characters (usually '/').  The second
element is the replacement string, which may or may not end with the
delimiter.  The third element are any regex flags."
  (let* ((regex-string (read-regex-string port))
         (delimiter (string-ref-safe regex-string 0)))
    (cond
     ((char=? delimiter #\null)
      ;; The regex was invalid, so there is no need to look for a
      ;; replacement string.
      (set-err-msg! repl (get-read-regex-string-err))
      (set-status! repl ERR)
      #f)

     ;; FIXME: handle empty regex. Should I use the previous
     ;; regex in that case?

     (else
      ;; Trim off the delimiters.
      (set! regex-string
        (substring regex-string 1 (1- (string-length regex-string))))

      ;; A valid regex was found, so now look for a replacement
      ;; string.
      (let ((replacement-string (read-replacement-string port delimiter)))
        (cond
         ((eof-object? replacement-string)
          ;; No replacement string was found
          (set-err-msg! repl "no replacement string found")
          (set-status! repl ERR)
          #f)

         ((not replacement-string)
          ;; The replacement string was invalid.
          (set-err-msg! repl (get-read-replacement-string-err))
          (set-status! repl ERR)
          #f)

         ((and (or (string=? replacement-string "%")
                   (string=? replacement-string (string #\% delimiter)))
               (not (get-last-replacement-string repl)))
          ;; The replacement string is the special '%', which
          ;; means we should re-use the previous valid replacement
          ;; string, but no previous replacement string was found.
          (set-err-msg! repl "no previous valid replacement string")
          (set-status! repl ERR)
          #f)

         (else
          ;; A valid replacement string was found.
          ;; Trim off the delimiter.
          (let ((ends-with-delimiter?
                 (char=? delimiter
                         (string-ref replacement-string
                                     (1- (string-length replacement-string))))))
            (when ends-with-delimiter?
              (set! replacement-string (string-drop-right replacement-string 1)))
            ;; The replacement-string is the special '%', which
            ;; indicates that the previous replacement string should
            ;; be used.
            (when (string=? replacement-string "%")
              (set! replacement-string (get-last-replacement-string repl)))

            ;; If it ends with the delimiter, then search for flags
            (cond
             ((not ends-with-delimiter?)
              ;; It doesn't end with a delimiter, so there are no flags.
              (list regex-string replacement-string ""))

             (else
              ;; Search for flags.
              (list
               regex-string
               replacement-string
               (read-filter "0123456789glnp" port))))))))))))

(define-method (ed-repl-parse-shell (repl <EdRepl>) port)
  "Extracts a shell from the given port.  It will return the string,
or #f on failure."
  (let ((txt ""))
    (if (not (eof-object? (peek-char port)))
        (set! txt (read-whitespace port)))
    (if (eof-object? (peek-char port))
        (begin
          (set-err-msg! repl "missing shell command")
          (set-status! repl ERR)
          (unread-string txt port)
          #f)
        ;; else
        (string-trim-both (read-line port)))))

;; Arguments
;;
;; If an operation takes zero addresses but receives more than
;; zero addresses, it is an error.

;; If an operation takes more than zero addresses but receives
;; zero addresses, the defaults are used.

;; If an operation takes two addresses but receives one
;; address, apparently, the given address is used for both.
(define-method (ed-repl-unpack-addr-range (Repl <EdRepl>) Op AddrList)
  (warn-if-false (list-length-9? Op))
  (warn-if-false (list-of-integers? AddrList))

  (let ((addr-count-required (dispatch:addr-count Op))
        (addr-list-len (length AddrList))
        (line-cur (ed-repl-get-line-cur-in-ed-coordinates Repl))
        (line-last (ed-repl-get-line-cur-in-ed-coordinates Repl)))

    (cond
     ((and (= addr-count-required 0) (> addr-list-len 0))
      (set-err-msg! Repl
                    (format #f "command '~a' expects zero addresses" (dispatch:key Op)))
      #f)
     (else
      (let ((addr
             (cond
              ((= addr-count-required 0)
               '())
              ((= addr-count-required 1)
               (cond
                ((>= addr-list-len 1)
                 (list (last AddrList)))
                (else
                 (list (convert-default-addr (dispatch:addr-start Op) line-cur line-last)))))
              ((= addr-count-required 2)
               (cond
                ((= addr-list-len 2)
                 AddrList)
                ((= addr-list-len 1)
                 (list (last AddrList)
                       (last AddrList)))
                ((= addr-list-len 0)
                 (list (convert-default-addr (dispatch:addr-start Op) line-cur line-last)
                       (convert-default-addr (dispatch:addr-end Op) line-cur line-last)))))
              (else
               ;; Should never reach here.
               (set-err-msg! Repl
                             (format #f "command ~a expects ~a addresses but received ~a addresses"
                                     (dispatch:key Op) addr-count-required addr-list-len))
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

(define-method (op-append (Repl <EdRepl>) AddrList _ Suffix StrList)
  "Appends StrList, a list of zero or more strings, after the
1-indexed line number held as the 1st element of AddrList.  That
address can be zero, which means the text will be inserted before the
1st line.

The current line number will become the address of the last inserted
line, or, if STRLIST was an empty list, the addressed line.

A Suffix of 'l', 'n', or 'p' is allowed."
  (warn-if-false (list-of-integers-length-1+? AddrList))
  (warn-if-false (list-of-strings-length-0+? StrList))
  (warn-if-false (string? Suffix))
  ;; The CBuffer primitive 'insert-lines' expects the line number
  ;; where the insertions happens, and its line numbers are
  ;; zero-indexed.

  ;; Ed line numbers are 1-indexed and indicate the line after which
  ;; the insertion occurs. Zero means before the first line.

  ;; So in this case Ed address == CBuffer address
  (insert-lines Repl (first AddrList) StrList)
  (unless (null? StrList)
    (set-modified! Repl #t))
  (unless (string-null? Suffix)
    ;; When displaying a line after an append, print only the current
    ;; line.
    (ed-repl-display-lines Repl
                           (+ 1 (get-line-cur Repl))
                           (+ 1 (get-line-cur Repl))
                           Suffix))
  0)

(define-method (op-change (Repl <EdRepl>) AddrList Special Suffix StrList)
  "Changes the lines bracketed by the 1-indexed, inclusive line
numbers held as the two elements of AddrList. Those lines are deleted
and replaced with StrList, a list of zero or more strings.  An address
of zero is valid, and will be interpreted as address 1.

The current line number will become the address of the last changed
line, or, if STRLIST was an empty list, to the last deleted line.

A Suffix of 'l', 'n', or 'p' is allowed."
  (warn-if-false (list-of-integers-length-2? AddrList))
  (warn-if-false (list-of-strings? StrList))
  (warn-if-false (string? Suffix))

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive).

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). As an special case, zero is
  ;; mapped to one.
  (let ((start (1- (max 1 (first AddrList))))
        (end (max 1 (second AddrList))))
    (if (null? (get-text Repl))
        (insert-lines Repl start StrList)
        (replace-lines Repl start end StrList)))
  (set-modified! Repl #t)
  (unless (string-null? Suffix)
    (ed-repl-display-lines Repl
                           (1+ (get-line-cur Repl))
                           (1+ (get-line-cur Repl)) Suffix))
  0)

(define-method (op-copy (Repl <EdRepl>) AddrList Addr3 Suffix append)
  "Copies the addressed lines after the line addressed by the third address.
If the 3rd address is zero, it copies the addressed lines to the
beginning."
  (warn-if-false (cbuffer? Repl))
  (warn-if-false (list-of-integers-length-2? AddrList))
  (warn-if-false (integer-nonnegative? Addr3))
  (warn-if-false (string? Suffix))

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive), and will
  ;; copy to the zero-indexed 3rd address.

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). The 3rd address is the
  ;; 1-indexed line after which to move the lines.  Zero indicates
  ;; that the lines are to be inserted before the 1st line.
  (copy-lines Repl (1- (first AddrList)) (second AddrList) Addr3)
  (set-modified! Repl #t)
  (unless (string-null? Suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines Repl (1+ (get-line-cur Repl))
                           (1+ (get-line-cur Repl)) Suffix))
  0)

(define-method (op-delete (Repl <EdRepl>) AddrList Special Suffix Append)
  "Deletes the lines bracketed by the 1-indexed, inclusive
line numbers that are the two elements of AddrList.

The current line number will become the address of the line after
the last deleted line.  If the deleted lines are at the end of the buffer,
the address will be the last remaining line.

A Suffix if 'l', 'n', or 'p' is allowed."
  (warn-if-false (list-of-integers-length-2? AddrList))
  (warn-if-false (string? Suffix))

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive).

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). An address of zero is invalid.
  (delete-lines Repl (1- (first AddrList)) (second AddrList))
  (set-modified! Repl #t)
  (unless (string-null? Suffix)
    (ed-repl-display-lines Repl (1+ (get-line-cur Repl))
                           (1+ (get-line-cur Repl)) Suffix))
  0)

(define-method (op-edit (Repl <EdRepl>) AddrList FileNameOrCmd Suffix Append)
  "Replaces the entire contents of the buffer with either a file's
contents or the output from a shell command.  If the file has been
modified since its last save and the previous command was not 'e', the
command will be rejected with an error.  If no filename is given, the
previously used filename will be used; howver, if there is no
previously used filaname, the command will be rejected with an error.

The the tags and bookmarks will be cleared.
The line number will be set to the last line of the buffer."
  (warn-if-false (string? FileNameOrCmd))

  (if (and (get-modified Repl)
           (not (eqv? (get-last-command Repl) #\e)))
      (begin
        (set-err-msg! Repl "there are unsaved changes")
        (set-status! Repl ERR)
        ERR)
      ;; else
      (op-edit-without-checking Repl AddrList FileNameOrCmd Suffix Append)))

(define-method (op-edit-without-checking (Repl <EdRepl>) AddrList FileNameOrCmd Suffix Append)
  "Same as op-edit, except that it shall not check to see wheter any
changes have been made since the last 'w' write command."
  (warn-if-false (string? FileNameOrCmd))

  (let ((size 0)
        (port
         (if (string-starts-with? FileNameOrCmd #\!)
             (open-input-pipe (string-drop FileNameOrCmd 1))
             ;; else
             (false-if-exception (open-input-file (string-strip-escapes FileNameOrCmd))))))

    (cond
     ((not port)
      (set-err-msg! Repl (format #f "cannot open input '~a'" FileNameOrCmd))
      (set-status! Repl ERR)
      ERR)
     (else
      (set! size (replace-all-from-port Repl port))
      (cond
       ((< size 0)
        ERR)
       ((if (char=? (string-ref-safe FileNameOrCmd 0) #\!)
            (not (status:exit-val (close-pipe port)))
            ;; else
            (begin (close-input-port port) #f))
        (set-err-msg! Repl (format #f "cannot close input '~a'" FileNameOrCmd))
        (set-status! Repl ERR)
        ERR)
       (else
        ;; Success
        (set-modified! Repl #f)
        (unless (get-scripted Repl)
          (display size)
          (newline))
        0))))))

(define-method (op-filename (Repl <EdRepl>) AddrList FileName Suffix Append)
  "Store fname as a filename for future saving operations."
  ;; All the heavy lifting occurs in the parser.
  (warn-if-false (string? FileName))

  (display FileName)
  (newline)
  0)

(define-method (op-global (repl <EdRepl>) addr regex+cmd suffix append)
  (%op-global repl addr regex+cmd #t #f))

(define-method (op-global-non-matched (repl <EdRepl>) addr regex+cmd suffix append)
  (%op-global repl addr regex+cmd #f #f))

(define-method (op-global-interactive (repl <EdRepl>) addr regex suffix append)
  (%op-global repl addr (cons regex "") #t #t))

(define-method (op-global-interactive-non-matched (repl <EdRepl>) addr regex suffix append)
  (%op-global repl addr (cons regex "") #f #t))

(define-method (%op-global (repl <EdRepl>) addr regex+cmd match? interactive?)
  (let ((delimited-regex-string (car regex+cmd))
        (cmd-string (cdr regex+cmd)))
    (let ((regex-string
           (if (not delimited-regex-string)
               #f
               ;; else
               (if (= 2 (string-length delimited-regex-string))
                   (begin
                     ;; FIXME, set regex to last successful regex
                     (throw 'fixme))
                   ;; else
                   (substring delimited-regex-string
                              1
                              (1- (string-length delimited-regex-string)))))))
    (if regex-string
        (begin
          (if match?
              (ed-global-mark repl
                              (1- (first addr))
                              (second addr)
                              regex-string)
              ;; else
              (ed-global-mark-unmatched repl
                                        (1- (first addr))
                                        (second addr)
                                        regex-string))
          ;; N.B.  In this function below, we actually recurse back
          ;; into ed-repl-do, but this time we get the commands from a
          ;; the cmd-string instead of the current input port.
          (ed-global-for-each repl
                              (lambda (repl)
                                (ed-repl-do
                                 repl
                                 (if interactive?
                                     (begin
                                       (when (get-prompt-active repl)
                                         (set-prompt-string! repl "global>"))
                                       (display (get-text-cur repl))
                                       (newline)
                                       (current-input-port))

                                     cmd-string))))
          (set-prompt-string! repl "command>")
          (ed-global-clear repl))

        ;; else
        (begin
          (set-err-msg! repl (get-read-regex-string-err))
          (set-status! ERR)
          ERR)))))

(define-method (op-help (Repl <EdRepl>) addr special suffix append)
  "Display the last error message."
  (display (get-err-msg Repl))
  (newline))

(define-method (op-help-mode (Repl <EdRepl>) addr special suffix append)
  "Toggle verbose display of error messages."
  (set-garrulous! Repl (not (get-garrulous Repl))))

(define-method (op-insert (Repl <EdRepl>) AddrList Special Suffix StrList)
  "Appends StrList, a list of zero or more strings, before the
1-indexed line number held as the 1st element of AddrList.  That
address can be zero, which is an alias for address one.

The current line number will become the address of the last inserted
line, or, if STRLIST was an empty list, the addressed line.

A Suffix of 'l', 'n', or 'p' is allowed."
  (warn-if-false (list-of-integers-length-1+? AddrList))
  (warn-if-false (list-of-strings-length-0+? StrList))
  (warn-if-false (string? Suffix))
  ;; The CBuffer primitive 'insert-lines' expects the line number
  ;; where the insertions happens, and its line numbers are
  ;; zero-indexed.

  ;; Ed line numbers are 1-indexed and indicate the line after which
  ;; the insertion occurs. Zero means before the first line.

  ;; So in this case Ed address - 1 == CBuffer address
  (insert-lines Repl (max 0 (1- (first AddrList))) StrList)
  (unless (null? StrList)
    (set-modified! Repl #t))
  (unless (string-null? Suffix)
    ;; When displaying a line after an append, print only the current
    ;; line.
    (ed-repl-display-lines Repl
                           (+ 1 (get-line-cur Repl))
                           (+ 1 (get-line-cur Repl))
                           Suffix))
  0)

(define-method (op-join (Repl <EdRepl>) AddrList Special Suffix Append)
  "AddrList contains 1-indexed, inclusive coordinates that bracket
lines to be joined into a single line.  If the AddrList has the same
start and end address, this command does nothing and the current line is
unchanged.

If joining occurs, the current line is moved to the joined line;
otherwise, the current line is unchagned.

A Suffix of 'l', 'n', or 'p' is allowed."
  (warn-if-false (list-of-integers-length-2? AddrList))
  (warn-if-false (string? Suffix))
  (warn-if-false (<= (first AddrList) (second AddrList)))
  (warn-if-false (>= (first AddrList) 1))

  (cond
   ((> (second AddrList) (get-line-count Repl))
    (set-err-msg! Repl (format #f "invalid address ~s" AddrList))
    (set-status! Repl ERR)
    ERR)
   ((= (first AddrList) (second AddrList))
    ;; Nothing to do
    0)
   (else
    ;; The CBuffer primitive join-lines expects the a 0-indexed start
    ;; address (inclusive) and end address (exclusive) where the
    ;; join happens.

    ;; Ed line numbers are 1-indexed and are start (inclusive)
    ;; and end (inclusive). Zero is invalid.
    (join-lines Repl (1- (first AddrList)) (second AddrList))
    (set-modified! Repl #t)

    (unless (string-null? Suffix)
      ;; When displaying a line after an append, print only
      ;; the current line.
      (ed-repl-display-lines Repl (1+ (get-line-cur Repl))
                             (1+ (get-line-cur Repl)) Suffix))
    0)))

(define-method (op-line-number (repl <EdRepl>) addr special suffix append)
  "Print the addressed line."
  (display (last addr))
  (newline)
  0)

(define-method (op-list (repl <EdRepl>) addr special suffix append)
  "Display the addressed lines."
  (ed-repl-display-lines repl (first addr) (1+ (second addr)) "l")
  0)

(define-method (op-mark (Repl <EdRepl>) AddrList Name Suffix Append)
  ;; Unusually, the Ed Mark command doesn't change the current line.
  ;; Ed bookmarks are 1-indexed.  CBuffer bookmarks are zero-indexed.
  (warn-if-false (list-of-integers-length-1? AddrList))
  (warn-if-false (string? Name))
  (log-debug-pk Name)
  (bookmark-bol Repl (1- (first AddrList)) Name)
  (unless (string-null? Suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines Repl
                           (1+ (get-line-cur Repl))
                           (1+ (get-line-cur Repl))
                           Suffix))
  0)

(define (op-move Repl AddrList Addr3 Suffix append)
  "Moves the addressed lines after the line addressed by the third address.
If the 3rd address is zero, it moves the addressed lines to the beginning."

  (warn-if-false (cbuffer? Repl))
  (warn-if-false (list-of-integers-length-2? AddrList))
  (warn-if-false (integer-nonnegative? Addr3))
  (warn-if-false (string? Suffix))

  ;; The CBuffer primitive wants the zero-indexed start line
  ;; (inclusive) and zero-indexed end line (exclusive), and will
  ;; move to the zero-indexed 3rd address.

  ;; The Ed address is a 1-indexed start line (inclusive) and a
  ;; 1-indexed end line (inclusive). The 3rd address is the
  ;; 1-indexed line after which to move the lines.  Zero indicates
  ;; that the lines are to be inserted before the 1st line.
  (move-lines Repl (1- (first AddrList)) (second AddrList) Addr3)
  (set-modified! Repl #t)

  (unless (string-null? Suffix)
    ;; When displaying a line after an append, print only
    ;; the current line.
    (ed-repl-display-lines Repl (1+ (get-line-cur Repl))
                           (1+ (get-line-cur Repl)) Suffix))
  0)

(define-method (op-null (Repl <EdRepl>) AddrList Special Suffix Append)
  "Print the addressed line."
  (warn-if-false (list-of-integers-length-1? AddrList))

  ;; Ed addresses are 1-indexed.
  (if (or (< (first AddrList) 1) (> (first AddrList) (get-line-count Repl)))
      (begin
        (set-err-msg! Repl (format #f "invalid address ~a" (first AddrList)))
        (set-status! Repl ERR)
        ERR)
      ;; else
      (begin
        (set-line-cur! Repl (1- (first AddrList)))
        (ed-repl-display-lines Repl (1+ (get-line-cur Repl)) (1+ (get-line-cur Repl)) "p")
        0)))

(define-method (op-number (repl <EdRepl>) addr special suffix append)
  (ed-repl-display-lines repl (first addr) (second addr) "n")
  0)

(define-method (op-print (repl <EdRepl>) addr special suffix append)
  (ed-repl-display-lines repl (first addr) (1+ (second addr)) "p")
  0)

(define-method (op-prompt (repl <EdRepl>) addr special suffix append)
  (set-prompt-active! repl (not (get-prompt-active repl)))
  0)

(define-method (op-quit (repl <EdRepl>) addr special suffix append)
  (if (and (get-modified repl)
           (not (eqv? (get-last-command repl) #\q)))
      (begin
        (set-err-msg! repl "there are unsaved changes")
        (set-status! repl ERR)
        ERR)
      ;; else
      (set-status! repl QUIT)))

(define-method (op-quit-without-checking (repl <EdRepl>) addr special suffix append)
  (set-status! repl QUIT))

(define-method (op-read (Repl <EdRepl>) AddrList FileNameOrCmd Suffix Append)
  "Reads in the file named by FileNameOrCmd and inserts it after the
1-indexed line referenced in AddrList.  An address of zero indicates
before the first line.

If FileNameOrCmd begins with '!', the rest of that parameter will be
a shell command to be read in, instead.

If FileNameOrCmd is empty, the the currently remembered pathname will
be used.  If there is no currently remembered pathname, an error will
occur.

If FileNameOrCmd is a filename, and there is no currently remembered
pathname, this filename will be used as the currently remembered
pathname.

The active data position will be set to the end of the last line read
in."
  (warn-if-false (list-of-integers-length-1? AddrList))
  (warn-if-false (string? FileNameOrCmd))

  (let* ((fname (if (not (string-null? FileNameOrCmd))
                    FileNameOrCmd
                    ;; else
                    (if (not (string-null? (get-filename Repl)))
                        (get-filename)
                        ;; else
                        "")))
         (size 0)
         (port
          (if (string-starts-with? FileNameOrCmd #\!)
              (open-input-pipe (string-drop FileNameOrCmd 1))
              ;; else
              (false-if-exception (open-input-file (string-strip-escapes FileNameOrCmd))))))

    (cond
     ((not port)
      (set-err-msg! Repl (format #f "cannot open input '~a'" FileNameOrCmd))
      (set-status! Repl ERR)
      ERR)
     (else
      (set! size (insert-lines-from-port Repl (first AddrList) port))
      (cond
       ((< size 0)
        ERR)
       ((if (char=? (string-ref-safe FileNameOrCmd 0) #\!)
            (not (status:exit-val (close-pipe port)))
            ;; else
            (begin (close-input-port port) #f))
        (set-err-msg! Repl (format #f "cannot close input '~a'" FileNameOrCmd))
        (set-status! Repl ERR)
        ERR)
       (else
        ;; Success
        (set-modified! Repl #t)
        (unless (get-scripted Repl)
          (display size)
          (newline))
        (if (and (string-null? (get-filename Repl))
                 (not (string-starts-with? FileNameOrCmd #\!)))
            (set-filename! Repl FileNameOrCmd))

        0))))))

(define-method (op-shell-escape (repl <EdRepl>) addr shcmd suffix append)
  ;; So, if shcmd begins with '!', replace that with the text of the
  ;; last shell command.  The spec says "the unescaped character '%'"
  ;; shall be replaced with the remembered pathname.  So I guess we
  ;; should interpret some variety of string escapes.

  ;; FIXME: actually do the escape.
  (system shcmd)
  0)

(define-method (op-substitute (Repl <EdRepl>) AddrList RegexReplace suffix append)
  "Searched the addressed lines for occurrences of the given regular
expression and replace either the first or all (non-overlapped) matched strings
with the replacement."
  (warn-if-false (cbuffer? Repl))
  (warn-if-false (list-of-integers-length-2? AddrList))

  (let* ((flags (third RegexReplace))
         (LModified
          (substitute-in-lines Repl
                               (1- (first AddrList)) (second AddrList)
                               (first RegexReplace) (second RegexReplace) flags)))
    (if (and LModified
             (or (string-contains flags "l")
                 (string-contains flags "n")
                 (string-contains flags "p")))
        (ed-repl-display-lines Repl (1+ (get-line-cur Repl)) (1+ (get-line-cur Repl)) flags)))
  0)

;;(define-method (op-undo (Repl <EdRepl>) Addr Special Suffix Append)
(define (op-undo Repl Addr Special Suffix Append)
  "Undo the last mutator operation."
  (warn-if-false (cbuffer? Repl))
  (if (undo Repl)
      (begin
        (set-modified! Repl #t)
        (unless (string-null? Suffix)
          (ed-repl-display-lines Repl
                                 (+ 1 (get-line-cur Repl))
                                 (+ 1 (get-line-cur Repl))
                                 Suffix)))
      ;; else
      (begin
        (set-err-msg! Repl "could not undo")
        (set-status! Repl ERR))))

(define (op-write Repl AddrList FileNameOrCmd Suffix append)
  ;; This whole function is a bit garbage, because it tried too hard
  ;; to merge the popen and fopen into one path.
  (let ((size 0)
        (port
         (if (string-starts-with? FileNameOrCmd #\!)
             (open-output-pipe (string-drop FileNameOrCmd 1))
             ;; else
             (false-if-exception (open-output-file (string-strip-escapes FileNameOrCmd))))))

    (cond
     ((not port)
      (set-err-msg! Repl "cannot open output file")
      (set-status! Repl ERR)
      ERR)
     ((< ((lambda (x) (set! size x) x)
          (display-lines-to-port Repl (1- (first AddrList))
                                 (second AddrList) port)) 0)
      ERR)
     ((if (string-starts-with? FileNameOrCmd #\!)
          (not (status:exit-val (close-pipe port)))
          ;; else
          (begin (close-output-port port) #f))
      (set-err-msg! Repl "cannot close output file")
      (set-status! Repl ERR)
      ERR)
     (else
      ;; Success
      (set-modified! Repl #f)
      (get-modified Repl)
      (unless (get-scripted Repl)
        (format (current-error-port) "~a~%" size))
      0))))

(define rpl (make-ed-repl))
(set-dispatch-table!
 rpl
 `((#\a    1 dot   #f    #t null          #t #t  ,op-append)
   (#\c    2 dot   dot   #t null          #t #t  ,op-change)
   (#\d    2 dot   dot   #f null          #t #f  ,op-delete)
   (#\e    0 #f    #f    #f file          #f #f  ,op-edit)
   (#\E    0 #f    #f    #f file          #f #f  ,op-edit-without-checking)
   (#\f    0 #f    #f    #f file          #f #f  ,op-filename)
   (#\g    2 1     $     #f regex+cmd     #t #f  ,op-global)
   (#\G    2 1     $     #f regex         #t #f  ,op-global-interactive)
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
   (#\q    0 #f    #f    #f null          #f #f  ,op-quit)
   (#\Q    0 #f    #f    #f null          #f #f  ,op-quit-without-checking)
   (#\r    1 $     #f    #t file          #f #f  ,op-read)
   (#\s    2 dot   dot   #f regex+replace #t #f  ,op-substitute)
   (#\t    2 dot   dot   #t address       #t #f  ,op-copy)
   (#\u    0 #f    #f    #f null          #t #f  ,op-undo)
   (#\v    2 1     $     #f regex+cmd     #t #f  ,op-global-non-matched)
   (#\V    2 1     $     #f regex         #t #f  ,op-global-interactive-non-matched)
   (#\w    2 1     $     #f file          #f #f  ,op-write)
   (#\=    1 $     #f    #f null          #t #f  ,op-line-number)
   (#\!    0 #f    #f    #f shell         #f #f  ,op-shell-escape)
   (#\nul  1 dot+1 #f    #f null          #t #f  ,op-null)))

(while (ed-repl-do rpl (current-input-port))
  #t)
