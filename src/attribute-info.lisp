;;;; Copyright (c) 2012 Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>

;;;; This file is part of yuka.

;;;; yuka is free software; you can redistribute it and/or modify it under
;;;; the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.

;;;; yuka is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :yuka)

;;; Functions and structures required to deal with
;;; class/code/field/method attributes.

(defstruct code-attribute
  (max-stack 0 :type integer)
  (max-locals 0 :type integer)
  (code nil :type simple-array)
  (exception-table nil :type simple-array)
  (attributes nil :type simple-array))

(defstruct local-variable
  (start-pc 0 :type integer)
  (length 0 :type integer)
  (name-index 0 :type integer)
  (descriptor-index 0 :type integer)
  (index 0 :type integer))

(defstruct exception-table-entry
  (from 0 :type integer)
  (to 0 :type integer)
  (target 0 :type integer)
  (type 0 :type integer))

(declaim (inline read-info))
(defun read-info (stream count)
  (read-array stream count #'read-byte))

(defun read-code (stream len)
  (if (or (= len 0) (>= len +max-code-len+))
      (error "Invalid code length: ~a." len))
  (read-array stream len #'read-byte))

(defun exception-table-to-string (self constant-pool)
  (with-output-to-string (s)
    (format s "     from    to    target    type~%")
    (map nil #'(lambda (e)
                 (format s "     ~5a    ~5a    ~5a    ~20@a~%"
                         (exception-table-entry-from e)
                         (exception-table-entry-to e)
                         (exception-table-entry-target e)
                         (constant-pool-string-at constant-pool (exception-table-entry-type e))))
         self)))

(defun exception-table-find-handler (self ex-klass-name constant-pool)
  (let ((res nil))
    (dotimes (i (length self))
      (let ((et (aref self i)))
        (when (string= (constant-pool-string-at constant-pool (exception-table-entry-type et))
                       ex-klass-name)
          (setf res et)
          (return))))
    res))

(defun code-attribute-to-string (self constant-pool 
                                 to-string-fns)
  (with-output-to-string (s)
    (format s "  Code:~%    stack= ~a, locals= ~a~%"
	    (code-attribute-max-stack self) (code-attribute-max-locals self))
    (format s "~a~%" (opcode-to-string (code-attribute-code self)))
    (when (> (length (code-attribute-exception-table self)) 0)
      (format s "    ExceptionTable:~%~a~%" 
              (exception-table-to-string 
               (code-attribute-exception-table self) constant-pool)))
    (format s "~%    LineNumberTable:~%")
    (format s "~a~%" (funcall (car to-string-fns)
                              (code-attribute-attributes self) 
                              constant-pool
                              (cdr to-string-fns)))))

(declaim (inline read-exception-table-entry))
(defun read-exception-table-entry (stream)
  (make-exception-table-entry :from (read-u2 stream)
                              :to (read-u2 stream)
                              :target (read-u2 stream)
                              :type (read-u2 stream)))

(declaim (inline read-exception-table))
(defun read-exception-table (stream count)
  (read-array stream count #'read-exception-table-entry))

(defun read-line-number-table (stream count)
  (let ((tbl (make-array count)))
    (dotimes (i count)
      (setf (aref tbl i) (cons (read-u2 stream) (read-u2 stream))))
    tbl))

(declaim (inline read-local-variable-table-enrty))
(defun read-local-variable-table-enrty (stream)
  (make-local-variable :start-pc (read-u2 stream)
		       :length (read-u2 stream)
		       :name-index (read-u2 stream)
		       :descriptor-index (read-u2 stream)
		       :index (read-u2 stream)))

(declaim (inline read-local-variable-table))
(defun read-local-variable-table (stream count)
  (read-array stream count #'read-local-variable-table-enrty))

(defun read-verification-type-info (stream tag)
  (case tag
    ((0) 'top)
    ((1) 'integer)
    ((2) 'float)
    ((3) 'double)
    ((4) 'long)
    ((5) 'null)
    ((6) 'uninitialized-this)
    ((7) (cons 'object (read-u2 stream)))
    ((8) (cons 'uninitialized (read-u2 stream)))
    (t (error "Invalid verification type info tag: ~a~%" tag))))

(declaim (inline verification-type-info-tag))
(defun verification-type-info-tag (self)
  (if (consp self)
      (car self)
      self))

(declaim (inline verification-type-info-data))
(defun verification-type-info-data (self)
  (if (consp self)
      (cdr self)
      nil))

(declaim (inline read-verification-type-info))
(defun read-verification-type-infos (stream count)
  (read-array stream count #'(lambda (stream)
			       (read-verification-type-info 
				stream (read-byte stream)))))

(defun read-full-frame (stream)
  (cons (read-u2 stream)
	(cons (read-verification-type-infos stream (read-u2 stream))
	      (read-verification-type-infos stream (read-u2 stream)))))

(defun read-stack-map-frame (stream tag)
  (cond ((and (>= tag 0) (<= tag 63))
	 (cons 'same nil))
	((and (>= tag 64) (<= tag 127))
	 (cons 'same-locals-1-stack-item (read-verification-type-infos stream 1)))
	((= tag 247)
	 (cons 'same-locals-1-stack-item-extended (cons (read-u2 stream)
							(read-verification-type-infos stream 1))))
	((and (>= tag 248) (<= tag 250))
	 (cons 'chop (read-u2 stream)))
	((= tag 251)
	 (cons 'same-extended (read-u2 stream)))
	((and (>= tag 252) (<= tag 254))
	 (cons 'append (cons (read-u2 stream)
			     (read-verification-type-infos stream 1))))
	((= tag 255)
	 (cons 'full (read-full-frame stream)))
	(t (error "Invalid stack-map-frame tag: ~a~%" tag))))

(declaim (inline stack-map-frame-tag))
(defun stack-map-frame-tag (self)
  (car self))

(defun stack-map-frame-offset-delta (self)
  (if (consp (cdr self))
      (cadr self)
      -1))

(defun stack-map-frame-data (self)
  (if (consp (cdr self))
      (cddr self)
      (cdr self)))

(declaim (inline full-frame-locals))
(defun full-frame-locals (self)
  (car (stack-map-frame-data self)))

(declaim (inline full-frame-items))
(defun full-frame-items (self)
  (cdr (stack-map-frame-data self)))

(declaim (inline read-stack-map-table))
(defun read-stack-map-table (stream count)
  (read-array stream count #'(lambda (stream)
			       (read-stack-map-frame stream (read-byte stream)))))

(defun read-attribute-info (stream user-data)
  (let* ((constant-pool (car user-data))
         (read-code-attr-fn (cdr user-data))
         (name-index (read-u2 stream))
	 (attr-len (read-u4 stream))
	 (attr-name (constant-pool-string-at constant-pool name-index)))
    (cond ((string= attr-name "Code")
	   (cons 'code (funcall read-code-attr-fn stream constant-pool)))
	  ((string= attr-name "LineNumberTable")
	   (cons 'line-number-table (read-line-number-table stream (read-u2 stream))))
	  ((string= attr-name "LocalVariableTable")
	   (cons 'local-variable-table (read-local-variable-table stream (read-u2 stream))))
	  ((string= attr-name "LocalVariableTypeTable")
	   (cons 'local-variable-type-table (read-local-variable-table stream (read-u2 stream))))
	  ((string= attr-name "StackMapTable")
	   (cons 'stack-map-table (read-stack-map-table stream (read-u2 stream))))
	  (t (cons name-index (read-info stream attr-len))))))

(declaim (inline read-attribute-infos))
(defun read-attribute-infos (stream count constant-pool read-code-attr-fn)
  (read-array-with-user-data stream count 
                             (cons constant-pool read-code-attr-fn)
                             #'read-attribute-info))

(defun read-code-attribute (stream constant-pool)
  (make-code-attribute :max-stack (read-u2 stream)
		       :max-locals (read-u2 stream)
		       :code (bytes-to-opcode (read-code stream (read-u4 stream)))
		       :exception-table (read-exception-table stream (read-u2 stream))
		       :attributes (read-attribute-infos stream (read-u2 stream) 
                                                         constant-pool #'read-code-attribute)))

(defun line-number-table-to-string (self)
  (with-output-to-string (s)
    (dotimes (i (length self))
      (let ((a (aref self i)))
        (format s "      line ~a: ~a~%" (cdr a) (car a))))))

(defun verification-type-info-to-string (self constant-pool)
  (with-output-to-string (s)
    (let ((tag (verification-type-info-tag self)))
      (format s "~a " tag)
      (case tag
	((object) 
	 (format s "~a " (constant-pool-string-at 
			  constant-pool 
			  (verification-type-info-data self))))
	((uninitialized)
	 (format s "~a " (verification-type-info-data self)))))))

(defun verification-type-infos-to-string (self constant-pool)
  (format t "~a~%" self)
  (with-output-to-string (s)
    (dotimes (i (length self))
      (format s "~a" (verification-type-info-to-string (aref self i)
                                                       constant-pool)))))

(defun stack-map-frame-to-string (self constant-pool)
  (with-output-to-string (s)
    (let ((tag (stack-map-frame-tag self)))
      (format s "     frame_type = ~a~%" tag)
      (case tag
	((chop same-extended)
	 (format s "     offset_delta = ~a~%"
		 (stack-map-frame-offset-delta self)))
	((same-locals-1-stack-item)
	 (format s "      stack = ~a~%"
		 (verification-type-infos-to-string (stack-map-frame-data self) 
						    constant-pool)))
	((same-locals-1-stack-item-extended)
	 (format s "     offset_delta = ~a~%     stack = [~a]~%"
		 (stack-map-frame-offset-delta self)
		 (verification-type-infos-to-string (stack-map-frame-data self) 
						    constant-pool)))
	((append)
	 (format s "     offset_delta = ~a~%     locals = [~a]~%"
		 (stack-map-frame-offset-delta self)
		 (verification-type-infos-to-string (stack-map-frame-data self) 
						    constant-pool)))
	((full)
	 (format s "     offset_delta = ~a~%     stack = [~a]~%     locals = [~a]~%"
		 (stack-map-frame-offset-delta self)
		 (verification-type-infos-to-string (full-frame-items self) 
						    constant-pool)
		 (verification-type-infos-to-string (full-frame-locals self) 
						    constant-pool)))
	(t (error "Invalid stack-map-frame-tag: ~a~%" tag))))))

(defun stack-map-table-to-string (self constant-pool)
  (with-output-to-string (s)
    (format s "     StackMapTable: number_of_entries = ~a~%" (length self))
    (dotimes (i (length self))
      (let ((frame (aref self i)))
        (format s "    ~a~%" (stack-map-frame-to-string frame constant-pool))))))

(defmacro attribute-info-tag (self)
  `(car ,self))

(defmacro attribute-info-info (self)
  `(cdr ,self))

(defmacro is-code-attribute (self)
  `(eq 'code (attribute-info-tag ,self)))

(defun attribute-info-length (self)
  (length (attribute-info-info self)))

(defun attribute-infos-to-string (self constant-pool ainfo-to-string-fn)
  (with-output-to-string (s)
    (map 'nil #'(lambda (ainfo)
		  (format s "~%~a" 
                          (funcall ainfo-to-string-fn ainfo constant-pool)))
	 self)))

(defun attribute-info-to-string (self constant-pool)
  (let ((info (attribute-info-info self)))
    (case (attribute-info-tag self)
      ((code)
       (code-attribute-to-string info constant-pool 
                                 (cons #'attribute-infos-to-string
                                       #'attribute-info-to-string)))
      ((line-number-table)
       (line-number-table-to-string info))
      ((stack-map-table)
       (stack-map-table-to-string info constant-pool))
      (t info))))
