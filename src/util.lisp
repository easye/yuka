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

(defconstant +magic+ #xCAFEBABE)
(defconstant +max-code-len+ 65536)

(defun read-u2 (stream)
  (let ((i 0))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    i))

(defun read-u4 (stream)
  (let ((i 0))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    i))

(defun read-array (stream count reader-fn)
  (let ((data (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref data i) (funcall reader-fn stream)))
    data))

(defun read-array-with-user-data (stream count user-data reader-fn)
  (let ((data (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref data i) (funcall reader-fn stream user-data)))
    data))

(declaim (inline neq))
(defun neq (a b)
  (not (eq a b)))

(declaim (inline n=))
(defun n= (a b)
  (not (= a b)))

(defstruct iterator
  (sequence nil)
  (state nil)) ;; length, current index etc.

(defvar *eos* :eos) ;; end-of-sequence marker.

(defmacro eos-p (obj)
  `(eq ,*eos* ,obj))

(defstruct array-state
  (len 0 :type integer)
  (index 0 :type integer))

(defun make-array-iterator (array)
  (make-iterator :sequence array 
		 :state (make-array-state :len (length array))))

(defun iterator-next (self)
  (let* ((state (iterator-state self))
	 (i (array-state-index state)))
    (cond ((>= i (array-state-len state))
	   *eos*)
	  (t (let ((v (aref (iterator-sequence self) i)))
	       (setf (array-state-index state) (1+ i))
	       v)))))

(defmacro make-typed-array (count type-name)
  `(cons (make-array ,count) ,type-name))

(defmacro typed-array-elements (self)
  `(car ,self))

(defmacro typed-array-type-name (self)
  `(cdr ,self))
