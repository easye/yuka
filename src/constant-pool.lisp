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

(defconstant +nan+ 'nan)
(defconstant +positive-infinity+ 'infinity)
(defconstant +negative-infinity+ '-infinity)

(defmacro cp-info-tag (self)
  `(car ,self))

(defun read-klass-info (stream tag)
  (cons tag (read-u2 stream)))

(defmacro klass-info-tag (self)
  `(car ,self))

(defmacro klass-info-name-index (self)
  `(cdr ,self))

(defun read-xxxref-info (stream tag)
  (cons tag (cons (read-u2 stream) (read-u2 stream))))

(defmacro xxxref-info-tag (self)
  `(car ,self))

(defmacro xxxref-info-klass-index (self)
  `(cadr ,self))

(defmacro xxxref-info-name-type-index (self)
  `(cddr ,self))

(defun read-string-info (stream tag)
  (cons tag (read-u2 stream)))

(defmacro string-info-tag (self)
  `(car ,self))

(defmacro string-info-string-index (self)
  `(cdr ,self))

(defun read-integer-info (stream tag)
  (cons tag (read-u4 stream)))

(defmacro integer-info-tag (self)
  `(car ,self))

(defmacro integer-info-bytes (self)
  `(cdr ,self))

(defun read-float-info (stream tag)
  (cons tag (read-u4 stream)))

(defmacro float-info-tag (self)
  `(car ,self))

(defmacro float-info-bytes (self)
  `(cdr ,self))

(defun bits-to-float-value (bits)
  (let ((s (if (= 0 (ash bits -31)) 1 -1))
	(e (logand (ash bits -23) #xff))
	(m 0))
    (if (= e 0)
	(setf m (ash (logand bits #x7fffff) 1))
	(setf m (logior (logand bits #x7fffff) #x800000)))
    (* s m (expt 2 (- e 150)))))

(defun float-info-value (self)
  (let ((bits (float-info-bytes self)))
    (cond ((= bits #x7f800000) 
	   +positive-infinity+)
	  ((= bits #xff800000)
	   +negative-infinity+)
	  ((or (and (>= bits #x7f800001) (<= bits #x7fffffff))
	       (and (>= bits #xff800001) (<= bits #xffffffff)))
	   +nan+)
	  (t (bits-to-float-value bits)))))

(defun read-long-info (stream tag)
  (cons tag (cons (read-u4 stream) (read-u4 stream))))

(defmacro long-info-tag (self)
  `(car ,self))

(defmacro long-info-high-bytes (self)
  `(cadr ,self))

(defmacro long-info-low-bytes (self)
  `(cddr ,self))

(defmacro long-info-value (hi low)
  `(+ (ash ,hi 32) ,low))

(defun read-double-info (stream tag)
  (cons tag (cons (read-u4 stream) (read-u4 stream))))

(defmacro double-info-tag (self)
  `(car ,self))

(defmacro double-info-high-bytes (self)
  `(cadr ,self))

(defmacro double-info-low-bytes (self)
  `(cddr ,self))

(defun bits-to-double-value (bits)
  (let ((s (if (= (ash bits -63) 0) 1 -1))
	(e (logand (ash bits -52) #x7ff))
	(m 0))
    (if (= e 0)
	(setf m (ash (logand bits #xfffffffffffff) 1))
	(setf m (logior (logand bits #xfffffffffffff) #x10000000000000)))
    (coerce (* s m (expt 2 (- e 1075))) 'double-float)))

(defun double-info-value (self)
  (let ((bits (long-info-value (double-info-high-bytes self)
			       (double-info-low-bytes self))))
    (cond ((= bits #x7ff0000000000000)
	   +positive-infinity+)
	  ((= bits #xfff0000000000000)
	   +negative-infinity+)
	  ((or (and (>= bits #x7ff0000000000001) (<= bits #x7fffffffffffffff))
	       (and (>= bits #xfff0000000000001) (<= bits #xffffffffffffffff)))
	   +nan+)
	  (t (bits-to-double-value bits)))))

(defun read-name-and-type-info (stream tag)
  (cons tag (cons (read-u2 stream) (read-u2 stream))))

(defmacro name-and-type-info-tag (self)
  `(car ,self))

(defmacro name-and-type-info-name-index (self)
  `(cadr ,self))

(defmacro name-and-type-info-descriptor-index (self)
  `(cddr ,self))

(defun read-utf8-str (stream len)
  (let ((utf8-str (make-array len :element-type '(unsigned-byte 8))))
    (loop for i from 0 to (1- len) 
       do (setf (aref utf8-str i) (read-byte stream)))
    (trivial-utf-8:utf-8-bytes-to-string utf8-str)))

(defun read-utf8-info (stream tag)
  (let ((len (read-u2 stream)))
    (cons tag (cons len (read-utf8-str stream len)))))

(defmacro utf8-info-tag (self)
  `(car ,self))

(defmacro utf8-info-length (self)
  `(cadr ,self))

(defmacro utf8-info-str (self)
  `(cddr ,self))

(defun read-method-handle-info (stream tag)
  (cons tag (cons (read-byte stream) (read-u2 stream))))

(defmacro method-handle-info-tag (self)
  `(car ,self))

(defmacro method-handle-info-reference-kind (self)
  `(cadr ,self))

(defmacro method-handle-info-reference-index (self)
  `(cddr ,self))

(defun read-method-type-info (stream tag)
  (cons tag (read-u2 stream)))

(defmacro method-type-info-tag (self)
  `(car ,self))

(defmacro method-type-info-descriptor-index (self)
  `(cdr ,self))

(defun read-invoke-dynamic-info (stream tag)
  (cons tag (cons (read-u2 stream) (read-u2 stream))))

(defmacro invoke-dynamic-info-tag (self)
  `(car ,self))

(defmacro invoke-dynamic-info-bootstrap-method-attr-index (self)
  `(cadr ,self))

(defmacro invoke-dynamic-info-name-and-type-index (self)
  `(cddr ,self))
	       
(defun klass-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a#~a" "Class" (klass-info-name-index self))))

(defun fieldref-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a#~a.#~a" "Fieldref" (xxxref-info-klass-index self)
	    (xxxref-info-name-type-index self))))

(defun methodref-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a#~a.#~a" "Methodref" (xxxref-info-klass-index self)
	    (xxxref-info-name-type-index self))))

(defun interface-methodref-info-to-string (self)
  (with-output-to-string (s)
    (format s "~a (~a)" 'interface-method self)))

(defun string-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a#~a" "String" (string-info-string-index self))))

(defun integer-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a~a" "Integer" (integer-info-bytes self))))

(defun float-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a~ff" "Float" (float-info-value self))))

(defun long-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a~a" "Long" (long-info-value (long-info-high-bytes self)
						(long-info-low-bytes self)))))

(defun double-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a~a" "Double" (double-info-value self))))  

(defun name-and-type-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a#~a:#~a" "NameAndType" 
	    (name-and-type-info-name-index self)
	    (name-and-type-info-descriptor-index self))))

(defun utf8-info-to-string (self)
  (with-output-to-string (s)
    (format s "~20a~a" "Utf8" (utf8-info-str self))))

(defun method-handle-info-to-string (self)
  (with-output-to-string (s)
    (format s "~a (~a)" 'method-handle self)))

(defun method-type-info-to-string (self)
  (with-output-to-string (s)
    (format s "~a (~a)" 'method-type self)))

(defun invoke-dynamic-info-to-string (self)
  (with-output-to-string (s)
    (format s "~a (~a)" 'invoke-dynamic self)))

(defconstant +klass-info+ 7)
(defconstant +fieldref-info+ 9)
(defconstant +methodref-info+ 10)
(defconstant +interface-methodref-info+ 11)
(defconstant +string-info+ 8)
(defconstant +integer-info+ 3)
(defconstant +float-info+ 4)
(defconstant +long-info+ 5)
(defconstant +double-info+ 6)
(defconstant +name-and-type-info+ 12)
(defconstant +utf8-info+ 1)
(defconstant +method-handle-info+ 15)
(defconstant +method-type-info+ 16)
(defconstant +invoke-dynamic-info+ 18)

(defparameter *cp-info-fns*
  (list (cons +klass-info+ (cons #'read-klass-info #'klass-info-to-string))
	(cons +fieldref-info+ (cons #'read-xxxref-info #'fieldref-info-to-string))
	(cons +methodref-info+ (cons #'read-xxxref-info #'methodref-info-to-string))
	(cons +interface-methodref-info+ (cons #'read-xxxref-info #'interface-methodref-info-to-string))
	(cons +string-info+ (cons #'read-string-info #'string-info-to-string))
	(cons +integer-info+ (cons #'read-integer-info #'integer-info-to-string))
	(cons +float-info+ (cons #'read-float-info #'float-info-to-string))
	(cons +long-info+ (cons #'read-long-info #'long-info-to-string))
	(cons +double-info+ (cons #'read-double-info #'double-info-to-string))
	(cons +name-and-type-info+ (cons #'read-name-and-type-info #'name-and-type-info-to-string))
	(cons +utf8-info+ (cons #'read-utf8-info #'utf8-info-to-string))
	(cons +method-handle-info+ (cons #'read-method-handle-info #'method-handle-info-to-string))
	(cons +method-type-info+ (cons #'read-method-type-info #'method-type-info-to-string))
	(cons +invoke-dynamic-info+ (cons #'read-invoke-dynamic-info #'invoke-dynamic-info-to-string))))

(defun get-cp-info-fn (tag fn-picker)
  (let ((fn (assoc tag *cp-info-fns*)))
    (when (null fn)
      (error "Invalid constant pool tag: ~a~%" tag))
    (funcall fn-picker fn)))

(defun read-cp-info (stream tag)
  (funcall (get-cp-info-fn tag #'cadr) stream tag))

(defun read-constant-pool (stream count)
  (if (> count 0)
      (let ((cp (make-array count)))
        (loop for i from 0 to (1- count)
	   do (let ((tag (read-byte stream)))
		(setf (aref cp i) (read-cp-info stream tag))
		(when (or (eq tag +long-info+)
			  (eq tag +double-info+))
		  (setf i (1+ i)))))
        cp)
      (make-array 0)))

(defun cp-info-to-string (self)
  (funcall (get-cp-info-fn (cp-info-tag self) #'cddr) self))

(defun constant-pool-to-string (self)
  (with-output-to-string (s)
    (loop for i from 0 to (1- (length self))	 
       do (let ((cp-info (aref self i)))
	    (when (consp cp-info)
	      (format s "    #~2a = ~a~%" (1+ i) 
		      (cp-info-to-string cp-info)))))))

(defun constant-pool-string-at (self index)
  (let* ((cp-info (aref self (1- index)))
	 (tag (cp-info-tag cp-info)))
    (cond 
      ((= +utf8-info+ tag)
       (utf8-info-str cp-info))
      ((= +klass-info+ tag)
       (constant-pool-string-at self (klass-info-name-index cp-info)))
      ((= +methodref-info+ tag)
       (constant-pool-string-at self (xxxref-info-name-type-index cp-info)))
      ((= +name-and-type-info+ tag)
       (constant-pool-string-at self (name-and-type-info-name-index cp-info)))
      (t (error "(constant-pool-string-at ~a ~a) failed for for ~a." self index tag)))))
