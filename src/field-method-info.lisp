(in-package :yuka)

(defstruct field/method-info 
  (access-flags 0 :type integer)
  (name-index 0 :type integer)
  (descriptor-index 0 :type integer)
  (attributes-count 0 :type integer)
  (attributes nil :type simple-array))

(defun read-field/method-info (stream constant-pool)
  (let ((fi (make-field/method-info 
	     :access-flags (read-u2 stream)
	     :name-index (read-u2 stream)
	     :descriptor-index (read-u2 stream)
	     :attributes (read-attribute-infos stream (read-u2 stream) constant-pool))))
    (setf (field/method-info-attributes-count fi) 
          (length (field/method-info-attributes fi)))
    fi))

(defmacro read-field/method-infos (stream count constant-pool)
  `(read-array-with-user-data ,stream ,count ,constant-pool #'read-field/method-info))

(defun field/method-info-has-access-flag (self flag)
  (> (logand (field/method-info-access-flags self) flag) 0))

(defun field/method-info-to-string (self constant-pool)
  (with-output-to-string (s)
    (format s "~a~a ~a ~a" 
	    (flags-to-string (field/method-info-access-flags self))
	    (constant-pool-string-at constant-pool (field/method-info-descriptor-index self))
	    (constant-pool-string-at constant-pool (field/method-info-name-index self))
	    (attribute-infos-to-string (field/method-info-attributes self)))))
  
(defun field/method-infos-to-string (self constant-pool)
  (with-output-to-string (s)
    (map nil #'(lambda (info)
		 (format s "~a" (field/method-info-to-string info constant-pool)))
	 self)))

(defun method-code-attribute (self)
  (let ((iter (make-array-iterator (field/method-info-attributes self))))
    (loop (let ((a (iterator-next iter)))
	    (when (eq a *eos*)
	      (return nil))
	    (when (is-code-attribute a)
	      (return (attribute-info-info a)))))))
