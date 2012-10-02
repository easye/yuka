(in-package :yuka)

(defconstant +max-integer+ 2147483647)
(defconstant +min-integer+ (- +max-integer+))
(defconstant +max-long+ 9223372036854775807)
(defconstant +min-long+ (- +max-long+))
(defconstant +max-float+ 3.4028235E38)
(defconstant +min-float+ (- +max-float+))
(defconstant +max-double+ 1.7976931348623157D308)
(defconstant +min-double+ (1- +max-double+))

(defmacro make-int (v)
  `(cons 'int ,v))

(defmacro integer-info-to-int (iinfo)
  `(make-int (integer-info-bytes ,iinfo)))

(defmacro make-float (v)
  `(cons 'float ,v))

(defmacro float-info-to-float (finfo)
  `(make-float (float-info-value ,finfo)))

(defmacro make-double (v)
  `(cons 'double ,v))

(defmacro double-info-to-double (dinfo)
  `(make-double (double-info-value ,dinfo)))

(defmacro double-value (self)
  `(cdr ,self))

;; Implementation is FP-strict. (Section 2.8.2 of JVM Spec.)
(defun double-to-float (self)
  (let ((v (double-value self)))
    (cond ((symbolp v) ;; NaN, Infinity
           (make-float v))
          (t
           (if (and (>= v +min-float+)
                    (<= v +max-float+))
               (make-float v)
               (if (< v 0)
                   (make-float +min-float+)
                   (make-float +max-float+)))))))

(defun double-to-int (self)
  (let ((v (double-value self)))
    (cond ((symbolp v) ;; NaN, Infinity
           (make-int 0))
          (t
           (make-int
            (let ((iv (floor v)))
              (cond ((> iv +max-integer+)
                     +max-integer+)
                    ((< iv +min-integer+)
                     +min-integer+)
                    (t iv))))))))