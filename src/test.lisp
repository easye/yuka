(load "class-loader.lisp")

(defun skip-header (in)
  (read-u4 in) 
  (read-u2 in) 
  (read-u2 in) 
  (read-u2 in))

(setf in (open "../test/Test.class" :element-type '(unsigned-byte 8)))

(defparameter *cp* (read-constant-pool in (1- (skip-header in))))
(format t "Constant Pool:~%~a~%" (constant-pool-to-string *cp*))				  
(format t "Access Flags: ~a~%" (flags-to-string (read-u2 in)))
(format t "Class: ~a~%" (constant-pool-string-at *cp* (read-u2 in)))
(format t "Super: ~a~%" (constant-pool-string-at *cp* (read-u2 in)))
(format t "Interfaces: ")
(map nil #'(lambda (i)
	     (format t "~a " (constant-pool-string-at *cp* i)))
     (read-interfaces in (read-u2 in)))
(format t "~%")
(format t "Fields:~%~a~%" (field/method-infos-to-string 
			   (read-field/method-infos in (read-u2 in)) 
			   *cp*))
(format t "Methods:~%~a~%" (field/method-infos-to-string 
			    (read-field/method-infos in (read-u2 in)) 
			    *cp*))
(format t "Attributes:~%~a~%" (read-attribute-infos in (read-u2 in)))

(close in)
