;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg lists) - helper functions for lists
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
(define-module (mlg lists)
  #:use-module (srfi srfi-1)
  #:use-module (mlg logging)
  #:use-module (mlg typechecking)
  #:export (
            list-find-next-largest-entry
	    list-range-check-n?
	    list-range-check-right-n?
	    sublist
	    take-n
	    take-right-n
	    ))

(define (list-find-next-largest-entry lst val)
  "Given a list of numeric values, find the smallest entry
in lst that is greater than val."
  (fold (lambda (cur prev)
          (if (and (> cur val)
                   (or (not prev)
                       (< cur prev)))
              cur
              prev))
        #f
        lst))

(define (list-range-check-n? lst n low high)
  "Return #t if the first N elements in the list is between
low (inclusive) and high (inclusive).  If the list has fewer
than N elements, check all the elements in the list."
  (warn-if-false (list? lst))
  (warn-if-false (integer-nonnegative? n))
  (warn-if-false (number? low))
  (warn-if-false (number? high))

  (if (or (zero? n) (null? lst))
      #t
      ;; else
      (let ((sublist (take-n lst n)))
	(every (lambda (x)
		 (and (number? x) (<= low x) (<= x high)))
	       sublist))))

(define (list-range-check-right-n? lst n low high)
  "Return #t if the last N elements in the list is between
low (inclusive) and high (inclusive).  If the list has fewer
than N elements, check all the elements in the list."
  (warn-if-false (list? lst))
  (warn-if-false (integer-nonnegative? n))
  (warn-if-false (number? low))
  (warn-if-false (number? high))

  (if (or (zero? n) (null? lst))
      #t
      ;; else
      (let ((sublist (take-right-n lst n)))
	(every (lambda (x)
		 (and (number? x) (<= low x) (<= x high)))
	       sublist))))

(define (sublist lst start end)
  "Extract a sublist from lst between start (inclusive) and
end (exclusive)."
  (drop (take lst end) start))

(define (take-n lst n)
  "Return a list containing the first N elements of LST.  If the list
has N or fewer elements, return LST."
  (warn-if-false (list? lst))
  (if (> (length lst) n)
      (take lst n)
      lst))

(define (take-right-n lst n)
  "Return a list containing the lasn N elements of LST.  The return
value shares a common tail with LST. If the list has N or fewer elements,
return LST."
  (warn-if-false (list? lst))

  (if (> (length lst) n)
      (take-right lst n)
      lst))
