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

(defun is-refernce (obj)
  (format t "(is-refernce ~a) not implemented!~%" obj)
  nil)

(defun is-return-address (obj)
  (format t "(is-return-address ~a) not implemented!~%" obj)
  nil)

(defun resolve-field (objectref field-name)
  (format t "resolve-field: ~a in ~a not implemented!~%" 
	  objectref field-name)
  nil)

(defun resolve-static-field (objectref field-name)
  (format t "resolve-static-field: ~a in ~a not implemented!~%" 
	  objectref field-name)
  nil)