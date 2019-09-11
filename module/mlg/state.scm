;;; -*- mode: scheme; coding: us-ascii; indent-tabs-mode: nil; -*-
;;; (mlg state-machine) - a simple state machine framework
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

(define-module (gano state-machine)
  #:use-module (srfi srfi-1)
  #:export (make-state-machine
	    state-machine-do)
  )

;;; State Machine Framework

;;; Here we define a state machine vaguely like how
;;; Ragel defines its state machines.  It consists
;;; of three parts

;;; ACTION PROCEDURES

;;; The backbone of the state machine framework are action procedures.
;;; Each action procedure takes two parameters, VAL and CONTEXT.  VAL
;;; is an integer, or anything else that can be compared using '<' and
;;; '>'.  CONTEXT is any free form information, or #f, if there is not
;;; context.  The action procedure will return a new CONTEXT.

;;; ACTION LIST
;;; (list (ACTION_ID ACTION_NAME ACTION_PROC)
;;;        ...)
;;;
;;; An action list is an association list of action procedures.
;;; Each procedure is associated with a unique integer ACTION_ID
;;; and a informative ACTION_NAME.

;;; ACTION TABLE
;;; (list (ACTION_TABLE_ID (list ACTION_ID ...))
;;;       ...)
;;;
;;; An action table associates a unique ACTION_TABLE_ID
;;; to a list of ACTION_IDs.  It indicates that at a given
;;; point in the state machine, the procedures associated
;;; with the ACTION_IDs are to be executed, in order.

;;; STATE TRANSITION LIST
;;; (list (STATE_ID
;;;        (list (LOWER_BOUND UPPER_BOUND NEW_STATE_ID ACTOIN_TABLE_ID)
;;;               ...)
;;;       ...)

;;; Each entry in a state transition list has a LOWER_BOUND, and
;;; UPPER_BOUND, a NEW_STATE_ID, and an ACTION_TABLE_ID.  They
;;; represent that, for a given state, if the next input VAL is
;;; between LOWER_BOUND (inclusive) and UPPER_BOUND (inclusive) the
;;; VAL and the CONTEXT will be sent, in order, to the procedures
;;; referenced by the ACTION_TABLE_ID, and then afterward, the state
;;; machine will transition to the NEW_STATE_ID.

;;; HOW IT WORKS

;;; There is a starting state.
;;; When a value is received, the state transitions for the state
;;; are searched for a match to the new value.  If there is a match,
;;; the state machine engine calls the zero or more ACTION procedures
;;; and then jump to the new STATE ID.

;;; If a value is received that doesn't match any of the state
;;; transition ranges, a 'state-machine-error' is thrown, and
;;; the state machine stays in the same state.

;;; To use this module, first call 'state-machine-init'.  It
;;; will return the STATE_ID of the initial state.

;;; To use the state machine, call (state-machine-do machine state-id
;;; val context).  This will search the state machine for a transition
;;; from state-id to a new state given the input VAL.  The form and
;;; format of CONTEXT is not specified here. If it finds a transition,
;;; it will run the zero or more ACTION procedures, passing them (val
;;; context).  'state-machine-do' will return two values of the new
;;; state ID and the new context.  If the returned state ID is #f,
;;; instead of an integer, it means that the state machine has
;;; transitioned into a state that has no defined transitions.

(define (make-state-machine action-list action-table state-transition-list
			    initial-state)

  ;; Returns
  (cons initial-state . EMPTY_CONTEXT))

(define (state-machine-do machine state-id val context)

  ;; Returns
  (cons new-state-id . new-context))

