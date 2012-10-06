;; Copyright (c) 2012 Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>

;; This file is part of yuka.

;; yuka is free software; you can redistribute it and/or modify it under
;; the terms of the GNU Lesser General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.

;; yuka is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :yuka)

(defstruct bounded-stack
  (elements nil :type simple-array)
  (index 0 :type integer))

(defmacro make-stack (max-depth)
  `(make-bounded-stack :elements (make-array ,max-depth)))

(defun stack-push (self e)
  (let ((elements (bounded-stack-elements self))
	(index (bounded-stack-index self)))
    (when (>= index (length elements))
      (error "stack overflow."))
    (when (< index 0)
      (setf index 0))
    (setf (aref elements index) e)
    (setf (bounded-stack-index self) (1+ index)))
  self)

(defun stack-pop (self)
  (let ((elements (bounded-stack-elements self))
	(index (1- (bounded-stack-index self))))
    (when (< index 0)
      (error "stack underflow."))
    (setf (bounded-stack-index self) index)
    (aref elements index)))

(defun stack-top (self)
  (let ((e (stack-pop self))
        (i (bounded-stack-index self)))
    (setf (bounded-stack-index self) (1+ i))
    e))

(defmacro stack-length (self)
  `(bounded-stack-index ,self))

(defun stack-at (self index)
  (let ((len (stack-length self)))
    (when (or (>= index len) (< index 0))
      (error "invalid stack index."))
    (aref (bounded-stack-elements self) (- (1- len) index))))

(defmacro stack-reset (self)
  `(setf (bounded-stack-index ,self) 0))

(defmacro stack-dup (self)
  `(stack-push ,self (stack-top ,self)))

(defun stack-dup-x1 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self)))
    (stack-push (stack-push (stack-push self v1) v2) v1)))

(defun stack-dup-x3 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self))
	(v3 (stack-pop self)))
    (stack-push (stack-push (stack-push (stack-push self v1) v3) v2) v1)))

(defun stack-dup-x2 (self)
  (if (and (is-category-1 (stack-at self 0))
	   (is-category-1 (stack-at self 1)))
      (stack-dup-x3 self)
      (stack-dup-x1 self)))

(defun stack-dup2 (self)
  (if (is-category-2 (stack-at self 0))
      (stack-dup self)
      (let ((v1 (stack-at self 0))
	    (v2 (stack-at self 1)))
	(stack-push (stack-push self v2) v1))))

(defun stack-dup2-x1-form2 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self)))
    (stack-push (stack-push (stack-push self v1) v2) v1)))

(defun stack-dup2-x1 (self)
  (if (is-category-2 (stack-at self 0))
      (stack-dup2-x1-form2 self)
      (let ((v1 (stack-pop self))
	    (v2 (stack-pop self))
	    (v3 (stack-pop self)))
	(stack-push (stack-push (stack-push (stack-push (stack-push self v2) v1) v3) v2) v1))))

(defun stack-dup2-x2-form1 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self))
	(v3 (stack-pop self))
	(v4 (stack-pop self)))
    (stack-push (stack-push (stack-push
			     (stack-push (stack-push (stack-push self v2) v1) v4)
			     v3) v2) v1)))

(defun stack-dup2-x2-form2 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self))
	(v3 (stack-pop self)))
    (stack-push (stack-push (stack-push (stack-push self v1) v3) v2) v1)))

(defun stack-dup2-x2-form3 (self)
  (let ((v1 (stack-pop self))
	(v2 (stack-pop self))
	(v3 (stack-pop self)))
    (stack-push (stack-push (stack-push (stack-push (stack-push self v2) v1) v3) v2) v1)))
	
(defun stack-dup2-x2 (self)
  (cond ((is-category-2 (stack-at self 0))
	 (if (is-category-1 (stack-at self 1))
	     (stack-dup2-x2-form2 self)
	     (stack-dup2-x1-form2 self)))
	((and (is-category-1 (stack-at self 0))
	      (is-category-1 (stack-at self 1)))
	 (if (is-category-1 (stack-at self 2))
	     (stack-dup2-x2-form1 self)
	     (stack-dup2-x2-form3 self)))))