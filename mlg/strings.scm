;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg strings) - more helper functions for strings
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

(define-module (mlg strings)
  #:use-module (mlg characters)
  #:use-module (ice-9 regex)
  #:use-module (mlg assert)
  #:use-module (mlg logging)
  #:export (string-crc16
            string-ends-with?
            string->format-escaped-string
            string-get-first-digit
            string-ref-safe
            string-sed-substitute
            string-starts-with?
            string-strip-escapes
            string->ed-escaped-string))

(define (string-ends-with? str char)
  "Return #t is the last character in string STR
is CHAR."
  (assert (string? str))
  (assert (char? char))
  (if (string-null? str)
      #f
      (char=? char (string-ref str (1- (string-length str))))))

(define (string->format-escaped-string str)
  "Return a new string that has (ice-9 format) codes escaped."
  (string-fold
   (lambda (c prev)
     (cond
      ((char=? c #\~)
       (string-append prev (string #\~ #\~)))
      (else
       (string-append prev (string c)))))
   ""
   str))

(define (string-get-first-digit str)
  "Returns an integer from zero to nine which indicates
the first character '0' through '9' found in the string.
If there are no characters '0' through '9' in the string,
it returns #f."
  (let ((len (string-length str)))
    (let loop ((i 0))
      (if (= i len)
          #f
          ;; else
          (let ((x (char->numeric-value (string-ref str i))))
            (if x
                x
                (loop (1+ i))))))))

(define (string-ref-safe str i)
  (if (or (not (string? str))
          (not (and (integer? i) (>= i 0))))
      #\null
      ;; else
      (if (>= i (string-length str))
          #\null
          ;; else
          (string-ref str i))))

(define (string-sed-substitute String RegexString ReplacementString FlagsString)
  "Given a regular expression string, a replacement string,
and flags, return a new string modified by the given subsitututon.

An amersand '&' in the replacement string shall be replaced by the
string matching the regular expression string.  Backslash + digit
in the replacement string shall be replaced by the corresponding
back reference in the regular expression string, or by an empty
string if there is no norresponding back reference.

Flags is a string consisting of one or more of
- 'g' - globally substitute all non-overlapping instances of the regular
  expression.
- '1' to '9' - substitute only the Nth occurrence of the regular expression
"
  (let ((global? (string-contains FlagsString "g"))
        (count (string-get-first-digit FlagsString)))

    ;; Unless the user has explicitly requested a count, we assume
    ;; that we're matching the first match, or, if a 'g' flag was
    ;; given, we matching on all matches.

    (when (not count)
      (set! count 1))
    (let loop ((i 1)
               (start 0)
               (output ""))
      (let ((match-struct (string-match RegexString String start)))
        ;; (log-debug-locals)
        (cond
         ((not match-struct)
          ;; There is no match, so append the remainder of the input
          ;; string to the output string, and exit.
          (string-append output
                         (string-drop String start)))

         ((and (not global?)
               (not (= i count)))
          ;; Here were skipping this match occurrence, so copy
          ;; the string that made a match to the output, and loop.
          (loop (1+ i)
                (match:end match-struct)
                (string-append output
                               (substring String
                                          start
                                          (match:end match-struct)))))

         (else
          ;; We've made a match, so create a replacement string.
          (let ((replaced-string
                 (let loop2 ((j 0)
                             (rstr (substring String
                                              start
                                              (match:start match-struct))))
                   (let ((c (string-ref-safe ReplacementString j)))
                     ;; (log-debug-locals)
                     (cond
                      ((char=? c #\null)
                       rstr)
                      ((char=? c #\\)
                       (let ((c2 (string-ref-safe ReplacementString (1+ j))))
                         (cond
                          ((char=? c2 #\null)
                           ;; Backslash at the end of the string is used
                           ;; as a literal backslash.  We're done.
                           (string-append rstr "\\"))

                          ((char->numeric-value c2)
                           ;; Backslash + digit means we're going to
                           ;; include the Nth submatch here, then keep
                           ;; looping.
                           (let ((n (char->numeric-value c2)))
                             (loop2 (+ 2 j)
                                    (string-append
                                     rstr
                                     (if (< n (match:count match-struct))
                                         (substring String
                                                    (match:start match-struct n)
                                                    (match:end match-struct n))
                                         ;; else
                                         "")))))

                          ((char=? c2 #\n)
                           ;; Backslash + n means we're going to
                           ;; put a real newline here.
                           (string-append rstr "\n"))

                          (else
                           ;; Backslash + anything else is an escape?
                           ;; FIXME: check if it is really an escape
                           (loop2 (+ 2 j)
                                  (string-append
                                   rstr
                                   (format #f (string #\\ c2))))))))

                      ((char=? c #\&)
                       ;; Ampersand is replaced by the entire match
                       (loop2 (1+ j)
                              (string-append
                               rstr
                               (substring String
                                          (match:start match-struct)
                                          (match:end match-struct)))))

                      (else
                       ;; Otherwise, this character is not an escape
                       ;; character.
                       (loop2 (1+ j)
                              (string-append
                               rstr
                               (string c)))))))))
            ;; Append the replace string, and return to the main loop.
            (loop (1+ i)
                  (match:end match-struct)
                  (string-append
                   output
                   replaced-string)))))))))

(define (string-starts-with? str char)
  "Return #t is the first character in string STR
is CHAR."
  (assert (string? str))
  (assert (char? char))
  (if (string-null? str)
      #f
      (char=? char (string-ref str 0))))

(define (string-strip-escapes str)
  "Returns a copy of STR with backslash string escapes removed."
  (let* ((esc #f)
         (newstr (string-fold
                  (lambda (c prev)
                    (cond
                     ((and (char=? c #\\) (not esc))
                      (set! esc #t)
                      prev)
                     (else
                      (set! esc #f)
                      (string-append prev (string c)))))
                  ""
                  str)))
    (if esc
        (string-append newstr (string #\\))
        newstr)))

(define (string->ed-escaped-string str)
  "Converts a string into another string. Non-printable characters
will be converted into the escape sequences that POSIX 'ed' prefers.
Dollar signs will be escaped, as well."
  (string-fold
   (lambda (c prev)
     (cond
      ((char=? c #\\)
       (string-append prev (string #\\ #\\)))
      ((char=? c #\alarm)
       (string-append prev (string #\\ #\a)))
      ((char=? c #\backspace)
       (string-append prev (string #\\ #\b)))
      ((char=? c #\page)
       (string-append prev (string #\\ #\f)))
      ((char=? c #\return)
       (string-append prev (string #\\ #\r)))
      ((char=? c #\tab)
       (string-append prev (string #\\ #\t)))
      ((char=? c #\vtab)
       (string-append prev (string #\\ #\v)))
      ((char=? c #\$)
       (string-append prev (string #\\ #\$)))
      ((not (char-set-contains? char-set:printing c))
       (string-append prev (format #f "\\~3,'0o" (char->integer c))))
      (else
       (string-append prev (string c)))))
   ""
   (string-normalize-nfc str)))

(load-extension "libguile-mlg" "init_strings_lib")
