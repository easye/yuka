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

(defconstant +max-byte+ (1- (expt 2 7)))
(defconstant +min-byte+ (expt -2 7))
(defconstant +max-short+ (1- (expt 2 15)))
(defconstant +min-short+ (expt -2 15))
(defconstant +max-integer+ (1- (expt 2 31)))
(defconstant +min-integer+ (expt -2 31))
(defconstant +max-long+ (1- (expt 2 63)))
(defconstant +min-long+ (expt -2 63))
(defconstant +max-float+ (coerce (* (- 2 (expt 2 -23)) (expt 2 127)) 'double-float))
(defconstant +min-float+ (- +max-float+))
(defconstant +max-double+ (coerce (* (- 2 (expt 2 -52)) (expt 2 1023)) 'double-float))
(defconstant +min-double+ (- +max-double+))

(defconstant +negative-zero+ '-zero)
(defconstant +float-zero+ 0.0)
(defconstant +float-one+ 1.0)
(defconstant +float-two+ 2.0)

(declaim (inline in-range))
(defun in-range (n start end)
  (and (>= n start) (<= n end)))

(declaim (inline is-int-in-range))
(defun is-int-in-range (i s e)
  (and (integerp i) (in-range i s e)))

(declaim (inline is-float-in-range))
(defun is-float-in-range (f s e)
  (and (floatp f) (in-range f s e)))

(defun make-i (v s e)
  (if (is-int-in-range v s e)
       v
       (if (> v 0) e s)))

(defun make-f (v s e)
  (if (is-float-in-range v s e)
       v
       (if (> v 0) e s)))

(defun make-integer (v)
  (make-i v +min-integer+ +max-integer+))

(declaim (inline integer-info-to-int))
(defun integer-info-to-int (iinfo)
  (make-integer (integer-info-bytes iinfo)))

(defun make-long (v)
  (make-i v +min-long+ +max-long+))

(defun make-float (v)
  (make-f v +min-float+ +max-float+)) 

(declaim (inline float-info-to-float))
(defun float-info-to-float (finfo)
  (make-float (float-info-value finfo)))

(defun make-double (v)
  (make-f v +min-double+ +max-double+))

(declaim (inline is-nan))
(defun is-nan (v)
  (eq v +nan+))

(declaim (inline is-positive-infinity))
(defun is-positive-infinity (v)
  (eq v +positive-infinity+))

(declaim (inline is-negative-infinity))
(defun is-negative-infinity (v)
  (eq v +negative-infinity+))

(defun make-inf-from (inf)
  (if (is-positive-infinity inf)
      +positive-infinity+
      +negative-infinity+))

(declaim (inline double-info-to-double))
(defun double-info-to-double (dinfo)
  (make-double (double-info-value dinfo)))

(declaim (inline is-infinity))
(defun is-infinity (v)
  (or (is-positive-infinity v) 
      (is-negative-infinity v)))

(declaim (inline nanis-negative-infinity))
(defun nanis-negative-infinity (v)
  (or (is-nan v) 
      (is-infinity v)))

(declaim (inline is-boolean))
(defun is-boolean (obj)
  (or (null obj) (eq t obj)))

(declaim (inline is-byte))
(defun is-byte (obj)
  (is-int-in-range obj +min-byte+ +max-byte+))

(declaim (inline is-char))
(defun is-char (obj)
  (characterp obj))

(declaim (inline is-short))
(defun is-short (obj)
  (is-int-in-range obj +min-short+ +max-short+))

(declaim (inline is-int))
(defun is-int (obj)
  (is-int-in-range obj +min-integer+ +max-integer+))

(declaim (inline is-long))
(defun is-long (obj)
  (is-int-in-range obj +min-long+ +max-long+))

(declaim (inline is-float))
(defun is-float (obj)
  (is-float-in-range obj +min-float+ +max-float+))

(declaim (inline is-double))
(defun is-double (obj)
  (is-float-in-range obj +min-double+ +max-double+))

;; Implementation is FP-strict. (Section 2.8.2 of JVM Spec.)
(defun double-to-float (v)
  (cond ((nanis-negative-infinity v)
	 (make-float v))
	(t (if (and (>= v +min-float+)
		    (<= v +max-float+))
	       (make-float v)
	       (if (< v 0)
		   (make-float +min-float+)
		   (make-float +max-float+))))))

(defun double-to-number (v constructor 
			 max-val min-val
			 &optional (default-val 0))
  (cond ((nanis-negative-infinity v)
	 (funcall constructor default-val))
	(t (funcall constructor
		    (let ((iv (floor v)))
		      (cond ((> iv max-val)
			     max-val)
			    ((< iv min-val)
			     min-val)
			    (t iv)))))))

(declaim (inline double-to-int))
(defun double-to-int (self)
  (double-to-number self #'make-integer
                    +max-integer+ +min-integer+))

(declaim (inline double-to-long))
(defun double-to-long (self)
  (double-to-number self #'make-long 
                    +max-long+ +min-long+))

(declaim (inline are-opposite-infs))
(defun are-opposite-infs (v1 v2)
  (or (and (is-positive-infinity v1) 
           (is-negative-infinity v2))
      (and (is-negative-infinity v1) 
           (is-positive-infinity v2))))

(declaim (inline are-same-infs))
(defun are-same-infs (v1 v2)
  (or (and (is-positive-infinity v1) 
           (is-positive-infinity v2))
      (and (is-negative-infinity v1) 
           (is-negative-infinity v2))))

(defun add-f (v1 v2 constructor)
  (cond ((or (is-nan v1) (is-nan v2)
	     (are-opposite-infs v1 v2))
	 +nan+)
	((are-same-infs v1 v2)
	 (make-inf-from v1))
	((or (is-positive-infinity v1) (is-positive-infinity v2))
	 +positive-infinity+)
	((or (is-negative-infinity v1) (is-negative-infinity v2))
	 +negative-infinity+)
	;; TODO: positive and negative zeros
	(t (funcall constructor (+ v1 v2)))))

(defun double-add (v1 v2)
  (add-f v1 v2 #'make-double))

(defun compare-nan-infs (v1 v2 is-l)
  (cond ((or (is-nan v1) (is-nan v2))
	 (if is-l
	     -1
	     1))
	((or (and (is-positive-infinity v1) (is-positive-infinity v2))
	     (and (is-negative-infinity v1) (is-negative-infinity v2)))
	 0)
	((is-positive-infinity v1)
	 1)
	((is-negative-infinity v1)
	 -1)))

(defun double-compare (v1 v2 is-l)
  (if (and (numberp v1) (numberp v2))
      (cond ((> v1 v2) 1)
	    ((< v1 v2) -1)
	    (t 0))
      (compare-nan-infs v1 v2 is-l)))

(declaim (inline is-nzero))
(defun is-nzero (v)
  (eq v +negative-zero+))

(declaim (inline is-zero))
(defun is-zero (v)
  (or (zerop v) 
      (is-nzero v)))

(defun div-f (v1 v2 constructor)
    (cond ((or (or (is-nan v1) (is-nan v2))
	     (and (is-infinity v1) (is-infinity v2))
	     (and (is-zero v1) (is-zero v2)))
	 +nan+)
	((and (is-infinity v1) (numberp v2))
	 (if (and (is-positive-infinity v1) (> v2 0.0))
	     +positive-infinity+
	     +negative-infinity+))
	((and (is-infinity v2) (numberp v1))
	 (if (and (is-positive-infinity v1) (> v2 0.0))
	     +float-zero+
	     +negative-zero+))
	((and (is-zero v1) (numberp v2))
	 +float-zero+)
	((is-zero v2)
	 (if (and (numberp v2) (> v2 0.0))
             +positive-infinity+
	     +negative-infinity+))
	(t (funcall constructor (/ v1 v2)))))

(defun double-div (v1 v2)
  (div-f v1 v2 #'make-double))

(defun dbl-mul-with-inf (v infv)
  (cond ((is-zero v)
	 +nan+)
	((numberp v)
	 (if (> v 0)
	     +positive-infinity+
	     +negative-infinity+))
	(t (if (or (is-negative-infinity infv) (is-negative-infinity v))
	       +negative-infinity+
	       +positive-infinity+))))

(defun mul-f (v1 v2 constructor)
    (cond ((or (is-nan v1) (is-nan v2))
	 +nan+)
	((is-infinity v1)
	 (dbl-mul-with-inf v2 v1))	  
	((is-infinity v2)
	 (dbl-mul-with-inf v1 v2))
	(t (funcall constructor (* v1 v2)))))

(defun double-mul (v1 v2)
  (mul-f v1 v2 #'make-double))

(defun neg-f (v constructor)
  (cond ((is-nan v)
	 +nan+)
	((is-positive-infinity v)
	 +negative-infinity+)
	((is-negative-infinity v)
	 +positive-infinity+)
	((is-nzero v)
	 0.0)
	((zerop v)
	 +negative-zero+)
	(t (funcall constructor (- v)))))

(defun double-neg (v)
  (neg-f v #'make-double))

(declaim (inline is-finite))
(defun is-finite (self)
  (or (numberp self) (is-zero self)))

(defun rem-f (dividend divisor constructor)
  (cond ((or (or (is-nan dividend) (is-nan divisor))
	     (and (is-infinity dividend) 
		  (or (is-zero divisor)
		      (is-infinity divisor))))
	 +nan+)
	((or (and (is-finite dividend) 
		  (is-infinity divisor))
	     (and (is-zero dividend) 
		  (is-finite divisor)))
	 dividend)
	(t (funcall constructor (rem dividend divisor)))))

(defun double-rem (dividend divisor)
  (rem-f dividend divisor #'make-double))

(defun double-sub (a b)
  (make-double (- a b)))

(defun float-floor-helper (self min-val max-val constructor)
  (cond ((is-nan self)
	 0)
	((is-positive-infinity self)
	 max-val)
	((is-negative-infinity self)
	 min-val)
	(t (funcall constructor (floor self)))))

(defun float-to-integer (self type)
  (case type
    ((integer) 
     (float-floor-helper self +min-integer+ +max-integer+ #'make-integer))
    ((long)
     (float-floor-helper self +min-long+ +max-long+ #'make-long))))

(defun is-category-1 (obj)
  (or (is-boolean obj)
      (is-byte obj)
      (is-char obj)
      (is-short obj)
      (is-int obj)
      (is-float obj)
      (is-refernce obj)
      (is-return-address obj)))

(defun is-category-2 (obj)
  (or (is-long obj) 
      (is-double obj)))

(defun float-add (v1 v2)
  (add-f v1 v2 #'make-float))

(defun float-div (v1 v2)
  (div-f v1 v2 #'make-float))

(defun float-mul (v1 v2)
  (mul-f v1 v2 #'make-float))

(defun float-neg (v)
  (neg-f v #'make-float))

(defun float-rem (dividend divisor)
  (rem-f dividend divisor #'make-float))

(defun float-sub (a b)
  (make-float (- a b)))
