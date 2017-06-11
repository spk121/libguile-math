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
  #:export (
	    sublist
	    take-n
	    take-right-n
	    ))

(define (sublist list start end)
  (drop (take list end) start))

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
