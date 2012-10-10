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

(defun is-reference (obj)
  (format t "(is-reference ~a) not implemented!~%" obj)
  nil)

(defun is-return-address (obj)
  (if (consp obj)
      (eq 'returnAddress (car obj))
      nil))

(defun resolve-field (objectref field-name)
  (format t "resolve-field: ~a in ~a not implemented!~%" 
	  objectref field-name)
  nil)

(defun resolve-static-field (objectref field-name)
  (format t "resolve-static-field: ~a in ~a not implemented!~%" 
	  objectref field-name)
  nil)

(defmacro vm-panic (msg args)
  `(error "Fatal Error: ~a [~a]~%" ,msg ,args))

(defun is-instance-of (klass-name objectref)
  (format t "(is-instance-of ~a ~a) not implemented!~%"
	  klass-name objectref)
  nil)

(defun invoke-dynamic (call-site-spec operand-stack)
  (format t "(invoke-dynamic ~a ~a) not implemented!~%"
	  call-site-spec operand-stack))

(defun invoke-interface (method-spec operand-stack count)
  (format t "(invoke-interface ~a ~a ~a) not implemented!~%"
	  method-spec operand-stack count))

(defun invoke-special (method-spec operand-stack)
  (format t "(invoke-special ~a ~a) not implemented!~%"
	  method-spec operand-stack))

(defun invoke-static (method-spec operand-stack)
  (format t "(invoke-static ~a ~a) not implemented!~%"
	  method-spec operand-stack))

(defun invoke-virtual (method-spec operand-stack)
  (format t "(invoke-virtual ~a ~a) not implemented!~%"
	  method-spec operand-stack))

(declaim (inline make-return-address))
(defun make-return-address (address)
  (cons 'returnAddress address))

(defmacro return-address-value (self)
  `(cdr ,self))

