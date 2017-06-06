;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg typechecking) - some simple typechecking queries
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
(define-module (mlg typechecking)
  #:use-module (srfi srfi-1)
  #:export (
            integer-nonnegative?
            list-length-1?
            list-length-2?
            list-length-9?
            list-of-integers-length-1+?
            list-of-integers-length-1?
            list-of-integers-length-2?
            list-of-integers?
            list-of-strings-length-0+?
            list-of-strings?
            ))

(define (integer-nonnegative? x)
  (and (integer? x)
       (>= x 0)))

(define (list-length-1? x)
  "Return #t if x is a list with a single entry."
  (and (list? x)
       (= 1 (length x))))

(define (list-length-2? x)
  "Return #t if x is a list with two entries."
  (and (list? x)
       (= 2 (length x))))

(define (list-length-9? x)
  "Return #t if x is a list with nine entries."
  (and (list? x)
       (= 9 (length x))))

(define (list-of-integers-length-1+? x)
  "Return #t if x is a list of one or more integers."
  (and (list? x)
       (>= (length x) 1)
       (every integer? x)))

(define (list-of-integers-length-1? x)
  "Return #t if x is a list containing one integer."
  (and (list? x)
       (= (length x) 1)
       (integer? (list-ref x 0))))

(define (list-of-integers-length-2? x)
  "Return #t if x is a list containing exactly two integers."
  (and (list? x)
       (= 2 (length x))
       (every integer? x)))

(define (list-of-integers? x)
  "Return #t if x is either an empty list or a list of integers."
  (and (list? x)
       (or (null? x)
           (every integer? x))))

(define (list-of-strings-length-0+? x)
  "Return #t if x is either an empty list of a list of strings."
  (and (list? x)
       (or (null? x)
           (every string? x))))

(define (list-of-strings? x)
  "Return #t if x is either an empty list of a list of strings."
  (and (list? x)
       (or (null? x)
           (every string? x))))
