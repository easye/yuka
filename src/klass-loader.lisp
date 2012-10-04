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

;; Loads a binary file in the JVM class file format.

(defun read-interfaces (stream count)
  (let ((interfaces (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref interfaces i) (read-u2 stream)))
    interfaces))

(defun klass-from-stream (stream)
  (when (not (= (read-u4 stream) +magic+))
    (error "Not a valid klass. Magic check failed."))
  (let ((mav (read-u2 stream)) (miv (read-u2 stream))
	(cp (read-constant-pool stream (1- (read-u2 stream)))))
    (let ((self (make-klass :minor-version mav
			    :major-version miv
			    :constant-pool cp
			    :access-flags (read-u2 stream)
			    :this (read-u2 stream)
			    :super (read-u2 stream)
			    :interfaces (read-interfaces stream (read-u2 stream))
			    :fields (read-field/method-infos stream (read-u2 stream) cp)
			    :methods (read-field/method-infos stream (read-u2 stream) cp)
			    :attributes (read-attribute-infos stream (read-u2 stream) cp))))
      (setf (klass-constant-pool-count self) (length (klass-constant-pool self)))
      (setf (klass-interfaces-count self) (length (klass-interfaces self)))
      (setf (klass-fields-count self) (length (klass-fields self)))
      (setf (klass-methods-count self) (length (klass-methods self)))
      (setf (klass-attributes-count self) (length (klass-attributes self)))
      self)))

(defmacro verify-end-of-klass-stream (stream)
  `(when (not (null (read-byte ,stream nil)))
     (error "Class file verfication failed. Found extra bytes at end.")))

(defun load-klass-file (file-name)
  (with-open-file (stream file-name :element-type '(unsigned-byte 8))
    (let ((klass (klass-from-stream stream)))
      (verify-end-of-klass-stream stream)
      klass)))

(defmacro find-klass (klass-name)
  `(format t "(find-klass ~a) not implemented!~%" ,klass-name))
