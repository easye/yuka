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

(load "packages.lisp")

(in-package :yuka)

;; Required by the class-loader.
(load "trivial-utf-8.lisp")
(load "util.lisp")
(load "opc.lisp")
(load "access-flags.lisp")
(load "constant-pool.lisp")
(load "attribute-info.lisp")
(load "field-method-info.lisp")
(load "klass.lisp")
(load "klass-loader.lisp")

;; Required by the virtual machine.
(load "vm-util.lisp")
(load "numeric.lisp")
(load "bounded-stack.lisp")
(load "opc-impl.lisp")
(load "frame.lisp")

(defun execute-method (method klass)
  (frame-run (make-frame-from-code (method-code-attribute method)
                                   klass)))

(defun run-method (klass method-name)
  (let ((method (klass-find-method-by-name klass method-name)))
    (when (null method)
      (error "Error: Method  `~a` not found in class ~a.~%" 
	     method-name (klass-name klass)))
    (execute-method method klass)))

(defun run-main (klass)
  (run-method klass "main"))
