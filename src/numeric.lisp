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

(defconstant +max-integer+ 2147483647)
(defconstant +min-integer+ (- +max-integer+))
(defconstant +max-long+ 9223372036854775807)
(defconstant +min-long+ (- +max-long+))
(defconstant +max-float+ 3.4028235E38)
(defconstant +min-float+ (- +max-float+))
(defconstant +max-double+ 1.7976931348623157D308)
(defconstant +min-double+ (1- +max-double+))

(defun make-int (v)
  (cons 'int v))

(defmacro integer-info-to-int (iinfo)
  `(make-int (integer-info-bytes ,iinfo)))

(defun make-long (v)
  (cons 'long v))

(defmacro make-float (v)
  `(cons 'float ,v))

(defmacro float-info-to-float (finfo)
  `(make-float (float-info-value ,finfo)))

(defun make-double (v)
  (cond ((> v +max-double+)
	 (make-pinf-double))
	((< v +min-double+)
	 (make-ninf-double))
	(t (cons 'double v))))

(defmacro make-nan-double ()
  `(cons 'double *nan*))

(defmacro make-pinf-double ()
  `(cons 'double *p-inf*))

(defmacro make-ninf-double ()
  `(cons 'double *n-inf*))

(defvar *n-zero* 'n-zero)

(defmacro make-nzero-double ()
  `(cons 'double *n-zero*))

(defmacro make-inf-double-from (inf)
  `(if (+infp ,inf)
       (make-pinf-double)
       (make-ninf-double)))

(defmacro double-info-to-double (dinfo)
  `(make-double (double-info-value ,dinfo)))

(defmacro double-value (self)
  `(cdr ,self))

(defvar *dbl-0* (make-double 0.0))
(defvar *dbl-n0* (make-nzero-double))
(defvar *dbl-1* (make-double 1.0))
(defvar *dbl-nan* (make-nan-double))
(defvar *dbl-pinf* (make-pinf-double))
(defvar *dbl-ninf* (make-ninf-double))

(defmacro nanp (v)
  `(eq ,v *nan*))

(defmacro +infp (v)
  `(eq ,v *p-inf*))

(defmacro -infp (v)
  `(eq ,v *n-inf*))

(defmacro infp (v)
  `(or (+infp ,v) (-infp ,v)))

(defmacro nan-infp (v)
  `(or (nanp ,v) (infp ,v)))

;; Implementation is FP-strict. (Section 2.8.2 of JVM Spec.)
(defun double-to-float (self)
  (let ((v (double-value self)))
    (cond ((nan-infp v)
           (make-float v))
          (t (if (and (>= v +min-float+)
		      (<= v +max-float+))
		 (make-float v)
		 (if (< v 0)
		     (make-float +min-float+)
		     (make-float +max-float+)))))))

(defun double-to-number (self make-type-fn 
			 max-val min-val
			 &optional (default-val 0))
  (let ((v (double-value self)))
    (cond ((nan-infp v)
           (funcall make-type default-val))
          (t (funcall make-type
		      (let ((iv (floor v)))
			(cond ((> iv max-val)
			       max-val)
			      ((< iv min-val)
			       min-val)
			      (t iv))))))))
  
(defmacro double-to-int (self)
  `(double-to-number ,self #'make-int 
		     ,+max-integer+ ,+min-integer+))

(defmacro double-to-long (self)
  `(double-to-number ,self #'make-long 
		     ,+max-long+ ,+min-long+))

(defmacro are-opposite-infs (v1 v2)
  `(or (and (+infp ,v1) (-infp ,v2))
       (and (-infp ,v1) (+infp ,v2))))

(defmacro are-same-infs (v1 v2)
  `(or (and (+infp ,v1) (+infp ,v2))
       (and (-infp ,v1) (-infp ,v2))))

(defun double-add (a b)
  (let ((v1 (double-value a))
	(v2 (double-value b)))
    (cond ((or (nanp v1) (nanp v2)
	       (are-opposite-infs v1 v2))
	   *dbl-nan*)
	  ((are-same-infs v1 v2)
	   (make-inf-double-from v1))
	  ((or (+infp v1) (+infp v2))
	   *dbl-pinf*)
	  ((or (-infp v1) (-infp v2))
	   *dbl-ninf*)
	  ;; TODO: positive and negative zeros
	  (t (make-double (+ v1 v2))))))

(defun compare-nan-infs (v1 v2 is-l)
  (cond ((or (nanp v1) (nanp v2))
	 (if is-l
	     -1
	     1))
	((or (and (+infp v1) (+infp v2))
	     (and (-infp v1) (-infp v2)))
	 0)
	((+infp v1)
	 1)
	((-infp v1)
	 -1)))

(defun double-compare (a b is-l)
  (let ((v1 (double-value a))
	(v2 (double-value b)))
    (if (and (numberp v1) (numberp v2))
	(cond ((> v1 v2) 1)
	      ((< v1 v2) -1)
	      (t 0))
	(compare-nan-infs v1 v2 is-l))))

(defmacro dbl-nzerop (v)
  `(eq ,v ,*n-zero*))

(defmacro dbl-zerop (v)
  `(or (zerop ,v) (dbl-nzerop ,v)))

(defun double-div (a b)
  (let ((v1 (double-value a))
	(v2 (double-value b)))
    (cond ((or (or (nanp v1) (nanp v2))
	       (and (infp v1) (infp v2))
	       (and (dbl-zerop v1) (dbl-zerop v2)))
	   *dbl-nan*)
	  ((and (infp v1) (numberp v2))
	   (if (and (+infp v1) (> v2 0.0))
	       *dbl-pinf*
	       *dbl-nan*))
	  ((and (infp v2) (numberp v1))
	   (if (and (+infp v1) (> v2 0.0))
	       *dbl-0*
	       *dbl-n0*))
	  ((and (dbl-zerop v1) (numberp v2))
	   *dbl-0*)
	  ((dbl-zerop v2)
	   (if (and (numberp v2) (> v2 0.0))
	       *dbl-pinf*
	       *dbl-ninf*))
	  (t (make-double (/ v1 v2))))))

(defun dbl-mul-with-inf (v infv)
  (cond ((dbl-zerop v)
	 *dbl-nan*)
	((numberp v)
	 (if (> v 0)
	     *dbl-pinf*
	     *dbl-ninf*))
	(t (if (or (-infp infv) (-infp v))
	       *dbl-ninf*
	       *dbl-pinf*))))

(defun double-mul (a b)
  (let ((v1 (double-value a))
	(v2 (double-value b)))
    (cond ((or (nanp v1) (nanp v2))
	   *dbl-nan*)
	  ((infp v1)
	   (dbl-mul-with-inf v2 v1))	  
	  ((infp v2)
	   (dbl-mul-with-inf v1 v2))
	  (t (make-double (* v1 v2))))))

(defun double-neg (self)
  (let ((v (double-value self)))
    (cond ((nanp v)
	   *dbl-nan*)
	  ((+infp v)
	   *dbl-ninf*)
	  ((-infp v)
	   *dbl-pinf*)
	  ((dbl-nzerop v)
	   0.0)
	  ((zerop v)
	   *dbl-n0*)
	  (t (make-double (- v))))))

(defmacro dbl-finite-p (self)
  `(or (numberp ,self) (dbl-zerop ,self)))

(defun double-rem (a b)
  (let ((dividend (double-value a))
	(divisor (double-value b)))
    (cond ((or (or (nanp dividend) (nanp divisor))
	       (and (infp dividend) (or (dbl-zerop divisor)
					(infp divisor))))
	   *dbl-nan*)
	  ((or (and (dbl-finite-p dividend) (infp divisor))
	       (and (dbl-zerop dividend) (dbl-finite-p divisor)))
	   a)
	  (t (make-double (rem dividend divisor))))))

(defun double-sub (a b)
  (make-double (- (double-value a) (double-value b))))
