(in-package :yuka)

(defmacro read-info (stream count)
  `(read-array ,stream ,count #'read-byte))

(defstruct code-attribute
  (max-stack 0 :type integer)
  (max-locals 0 :type integer)
  (code nil :type simple-array)
  (exception-table nil :type simple-array)
  (attributes nil :type simple-array))

(defun read-code (stream len)
  (if (or (= len 0) (>= len +max-code-len+))
      (error "Invalid code length: ~a." len))
  (read-array stream len #'read-byte))

(defun code-attribute-to-string (self)
  (with-output-to-string (s)
    (format s "  Code:~%    stack= ~a, locals= ~a~%"
	    (code-attribute-max-stack self) (code-attribute-max-locals self))
    (format s "~a~%" (opcode-to-string (code-attribute-code self)))
    (format s "    LineNumberTable:~%")
    (format s "~a~%" (attribute-infos-to-string (code-attribute-attributes self)))))

(defun read-et-entry (stream)
  (list (cons (read-u2 stream) (read-u2 stream))
	(cons (read-u2 stream) (read-u2 stream))))

(defmacro read-exception-table (stream count)
  `(read-array ,stream ,count #'read-et-entry))

(defun read-line-number-table (stream count)
  (let ((tbl (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref tbl i) (cons (read-u2 stream) (read-u2 stream))))
    tbl))

(defun read-attribute-info (stream constant-pool)
  (let* ((name-index (read-u2 stream))
	 (attr-len (read-u4 stream))
	 (attr-name (constant-pool-string-at constant-pool name-index)))
    (cond ((string= attr-name "Code")
	   (cons :code (read-code-attribute stream constant-pool)))
	  ((string= attr-name "LineNumberTable")
	   (cons :line-number-table (read-line-number-table stream (read-u2 stream))))
	  (t
	   (cons name-index (read-info stream attr-len))))))

(defmacro read-attribute-infos (stream count constant-pool)
  `(read-array-with-user-data ,stream ,count ,constant-pool #'read-attribute-info))

(defun read-code-attribute (stream constant-pool)
  (make-code-attribute :max-stack (read-u2 stream)
		       :max-locals (read-u2 stream)
		       :code (bytes-to-opcode (read-code stream (read-u4 stream)))
		       :exception-table (read-exception-table stream (read-u2 stream))
		       :attributes (read-attribute-infos stream (read-u2 stream) constant-pool)))

(defun line-number-table-to-string (self)
  (with-output-to-string (s)
    (loop for i from 0 to (1- (length self))
       do (let ((a (aref self i)))
	    (format s "      line ~a: ~a~%" (cdr a) (car a))))))

(defmacro attribute-info-tag (self)
  `(car ,self))

(defmacro attribute-info-info (self)
  `(cdr ,self))

(defmacro is-code-attribute (self)
  `(eq :code (attribute-info-tag ,self)))

(defun attribute-info-length (self)
  (length (attribute-info-info self)))
  
(defun attribute-info-to-string (self)
  (let ((info (attribute-info-info self)))
    (case (attribute-info-tag self)
      ((:code)
       (code-attribute-to-string info))
      ((:line-number-table)
       (line-number-table-to-string info))
      (t 
       info))))

(defun attribute-infos-to-string (self)
  (with-output-to-string (s)
    (map 'nil #'(lambda (ainfo)
		  (format s "~a" (attribute-info-to-string ainfo)))
	 self)))
