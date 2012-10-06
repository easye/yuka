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

(defstruct klass 
  (minor-version 0 :type integer)
  (major-version 0 :type integer)
  (constant-pool-count 0 :type integer)
  (constant-pool nil :type simple-array)
  (access-flags 0 :type integer)
  (this 0 :type integer)
  (super 0 :type integer)
  (interfaces-count 0 :type integer)
  (interfaces nil :type simple-array)
  (fields-count 0 :type integer)
  (fields nil :type simple-array)
  (methods-count 0 :type integer)
  (methods nil :type simple-array)
  (attributes-count 0 :type integer)
  (attributes nil :type simple-array))

(defun klass-find-method-by-name (self name)
  (let ((methods (klass-methods self))
	(cp (klass-constant-pool self))
	(method nil))
    (loop for i from 0 to (1- (length methods))
       do (let ((m (aref methods i))) 
	    (when (string= name (constant-pool-string-at 
				 cp (field/method-info-name-index m)))
	      (setf method m)
	      (return))))
    method))

(defmacro klass-name (self)
  `(constant-pool-string-at (klass-constant-pool ,self) (klass-this ,self)))

(defun klass-to-string (self)
  (with-output-to-string (s)
    (let ((cp (klass-constant-pool self)))
      (format s "Version: ~a.~a~%" 
	      (klass-major-version self)
	      (klass-minor-version self))
      (format s "Class: ~a~%" (constant-pool-string-at cp (klass-this self)))
      (format s "Super: ~a~%" (constant-pool-string-at cp (klass-super self)))
      (format s "Interfaces: ")
      (map nil #'(lambda (i)
		   (format s "~a " (constant-pool-string-at cp i)))
	   (klass-interfaces self))
      (format s "~%Fields:~%~a~%" (field/method-infos-to-string 
				   (klass-fields self) cp))
      (format s "Methods:~%~a~%" (field/method-infos-to-string 
				  (klass-methods self) cp))
      (format s "Constant Pool:~%~a~%" (constant-pool-to-string cp))
      (format s "Attributes: ~a~%" (klass-attributes self)))))
