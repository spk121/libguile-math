;;; -*- coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg ed filename) - filename help functions
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
(define-module (mlg ed filename)
  #:use-module (ice-9 optargs)
  #:use-module (ice-9 rdelim)
  #:use-module (mlg strings)
  #:use-module (mlg port)
  #:export (read-ed-filename
            get-read-ed-filename-err
            get-last-filename))

(define *last-filename* "")

(define (get-last-filename)
  *last-filename*)

(define (set-last-filename str)
  (set! *last-filename* str))

(define *read-ed-filename-err* "")

(define (get-read-ed-filename-err)
  *read-ed-filename-err*)

(define* (read-ed-filename #:optional (port (current-input-port)))
  "Extracts a filename from the given port.  If there is not text to
be read in the port, it returns the previously used filename.  If the
first character in the filename is '!', it reads a shell command
instead.  It will return the string, or #f on failure.  The
'get-read-ed-filename-err' will return a string describing the last
error."
  (let ((txt ""))
    (if (not (eof-object? (peek-char port)))
        (set! txt (read-whitespace port)))
    (if (eof-object? (peek-char port))
        (if (string-null? *last-filename*)
            (begin
              (set! *read-ed-filename-err* "missing filename")
              (unread-string txt port)
              #f)
            ;; else
            *last-filename*)
        ;; else
        (let ((str (string-trim-both (read-line port))))
          (if (not (string-starts-with? str #\!))
              (set! *last-filename* str))
          str))))

#!
(define (get-shell-command _str)
  "Strip backslash escapes. Replace '%' with the current
filename. Replace '!' with the last executed shell command."
  (let* ((stripped-str (string-strip-escapes _str))
         (ret 0)
         (shell-str (string-fold
                     (lambda (c prev)
                       (cond
                        ((char=? c #\!)
                         (if (string-null? shcmd)
                             (begin
                               (seterrmsg "no previous command")
                               (set! ret ERR))
                             ;; else
                             (set! ret 1)))
                        (string-append prev shcmd)
                        ((char=? c #\%)
                         (if (string-null? old-filename)
                             (begin
                               (seterrmsg "no current filename")
                               (set! ret ERR))
                             ;;
                             (set! ret 1))
                         (string-append prev old-filename))
                        (else
                         (string-append prev (string c)))))
                     ""
                     stripped-str)))
    (if (>= ret 0)
        (set! shcmd shell-str))
    ret))

!#
