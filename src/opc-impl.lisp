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

(defmacro throw-exception (exception)
  `(error "~a" ,exception))

(defmacro check-not-null (obj)
  `(when (null ,obj)
     (throw-exception 'NullPointerException)))

(defmacro check-bounds (array index)
  `(when (>= ,index (length ,array))
     (throw-exception 'ArrayIndexOutOfBoundsException)))

(defmacro check-array-type (array types)
  `(format t "(check-array-type ~a ~a) not implemented!~%"
           ,array ,types))  

(defmacro check-array-can-store (array value)
  `(format t "(check-array-can-store ~a ~a) not implemented!~%"
           ,array ,value))  

(defmacro check-for-neg-array-size (size)
  `(when (< ,size 0)
     (throw-exception 'NegativeArraySizeException)))

(defmacro check-obj-type (obj types)
  `(format t "(check-obj-type ~a ~a) not implemented!~%" 
           ,obj ,types))

(defmacro check-subclass-of (objref super-classes)
  `(format t "(check-subclass-of ~a ~a) not implemented!~%"
           ,objref ,super-classes))

(defmacro can-cast (src target)
  `(format t "(can-cast ~a ~a) not implemented!~%" ,src ,target))

(defmacro check-array (array index)
  `(check-not-null ,array)
  `(check-bounds ,array ,index))

(defmacro storev (locals operand-stack index)
  `(setf (aref ,locals ,index) (stack-pop ,operand-stack)))

(defun aaload (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack)))
    (check-array array index)
    (stack-push operand-stack (aref array index))))

(defun aastore (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack))
        (value (stack-pop operand-stack)))
    (check-array array index)
    (check-array-can-store array value)
    (setf (aref array index) value)))

(defmacro aload (locals operand-stack index)
  `(stack-push ,operand-stack (aref ,locals ,index)))

(defun anewarray (operand-stack constant-pool index)
  (let ((count (stack-pop operand-stack)))
    (check-for-neg-array-size count)
    (stack-push operand-stack (make-typed-array 
                               count
                               (constant-pool-string-at constant-pool index)))))

(defun arraylength (operand-stack)
  (let ((array (stack-pop operand-stack)))
    (check-not-null array)
    (stack-push operand-stack (length (typed-array-elements array)))))

(defun astore (locals operand-stack index)
  (let ((ref (stack-pop operand-stack)))
    (check-obj-type ref '(return-address reference))
    (setf (aref locals index) ref)))

(defun athrow (operand-stack exception-table constant-pool)
  (let ((ref (stack-pop operand-stack)))
    (check-obj-type ref '(reference))
    (check-subclass-of ref '(Throwable))
    (let ((h (exception-table-find-handler exception-table (klass-name ref) constant-pool)))
      (cond ((null h)
             (stack-reset operand-stack)
             (stack-push operand-stack ref)
             -1)
            (t (et-entry-target h))))))

(defun array-load (operand-stack)
  (let ((array (stack-pop operand-stack))
	(index (stack-pop operand-stack)))
    (check-array array index)
    (stack-push operand-stack (aref array index))))

(defun array-store (operand-stack types)
  (let ((array (stack-pop operand-stack))
        (index (stack-pop operand-stack))
        (value (stack-pop operand-stack)))
    (check-array array index)
    (check-obj-type value types)
    (setf (aref array index) value)))

(defmacro baload (operand-stack)
  `(array-load ,operand-stack))

(defmacro bastore (operand-stack)
  `(array-store ,operand-stack '(byte boolean)))

(defmacro caload (operand-stack)
  `(array-load ,operand-stack))

(defmacro castore (operand-stack)
  `(array-store ,operand-stack '(char)))

(defun checkcast (operand-stack index constant-pool)
  (let ((objref (stack-top operand-stack)))
    (when (not (null objref))
      (let ((name (constant-pool-string-at constant-pool index)))
        (when (not (can-cast objref (find-klass name)))
          (stack-pop operand-stack)
          (throw-exception 'ClassCastException))))))

(defmacro d2f (operand-stack)
  `(stack-push ,operand-stack 
               (double-to-float (stack-pop ,operand-stack))))

(defmacro d2i (operand-stack)
  `(stack-push ,operand-stack
               (double-to-int (stack-pop ,operand-stack))))

(defmacro d2l (operand-stack)
  `(stack-push ,operand-stack
	       (double-to-long (stack-pop ,operand-stack))))

(defmacro dadd (operand-stack)
  `(stack-push ,operand-stack
	       (double-add (stack-pop ,operand-stack)
			   (stack-pop ,operand-stack))))

(defmacro daload (operand-stack)
  `(array-load ,operand-stack))

(defmacro dastore (operand-stack)
  `(array-store ,operand-stack '(double)))

(defmacro dcmp (operand-stack is-l)
  `(stack-push ,operand-stack 
	       (double-compare (stack-pop ,operand-stack)
			       (stack-pop ,operand-stack)
			       ,is-l)))

(defmacro arith-helper (operand-stack b a fn)
  `(stack-push ,operand-stack (funcall ,fn ,a ,b)))

(defmacro arith (operand-stack fn)
  `(arith-helper ,operand-stack
		 (stack-pop ,operand-stack)
		 (stack-pop ,operand-stack)
		 ,fn))

(defmacro ddiv (operand-stack)
  `(arith ,operand-stack #'double-div))

(defmacro loadv (locals operand-stack index)
  `(stack-push ,operand-stack (aref ,locals ,index)))

(defmacro dmul (operand-stack)
  `(arith ,operand-stack #'double-mul))

(defmacro dneg (operand-stack)
  `(stack-push ,operand-stack (double-neg (stack-pop ,operand-stack))))

(defmacro drem (operand-stack)
  `(arith ,operand-stack #'double-rem))

(defmacro dsub (operand-stack)
  `(arith ,operand-stack #'double-sub))

(defmacro f2i (operand-stack)
  `(stack-push ,operand-stack 
               (float-to-integer (stack-pop ,operand-stack) 'integer)))

(defmacro f2l (operand-stack)
  `(stack-push ,operand-stack 
               (float-to-integer (stack-pop ,operand-stack) 'long)))

(defmacro fadd (operand-stack)
  `(arith ,operand-stack #'float-add))

(defmacro faload (operand-stack)
  `(array-load ,operand-stack))

(defmacro fastore (operand-stack)
  `(array-store ,operand-stack '(float)))

(defmacro fdiv (operand-stack)
  `(arith ,operand-stack #'float-div))

(defmacro fmul (operand-stack)
  `(arith ,operand-stack #'float-mul))

(defmacro fneg (operand-stack)
  `(stack-push ,operand-stack (float-neg (stack-pop ,operand-stack))))

(defmacro frem (operand-stack)
  `(arith ,operand-stack #'float-rem))

(defmacro fsub (operand-stack)
  `(arith ,operand-stack #'float-sub))

(defun getfield (operand-stack constant-pool index)
  (let ((objref (stack-pop operand-stack))
	(sym (constant-pool-string-at constant-pool index)))
    (check-not-null objref)
    (stack-push operand-stack (resolve-field objref sym))))

(defun getstatic (operand-stack constant-pool index)
  (let ((objref (stack-pop operand-stack))
	(sym (constant-pool-string-at constant-pool index)))
    (check-not-null objref)
    (stack-push operand-stack (resolve-static-field objref sym))))

(defun find-pc (dest-opc-sym byte-code byte-code-len jump-to)
  (let ((pc (loop for i from 0 to (1- byte-code-len)
               do (let ((opc-offset (opcode-offset (aref byte-code i))))
                    (when (= opc-offset jump-to)
                      (return opc-offset))))))
    (when (null pc)
      (vm-panic "Invalid jump offset." (cons dest-opc-sym jump-to)))
    pc))

(defmacro goto (byte-code byte-code-len jump-to)
  `(find-pc 'goto ,byte-code ,byte-code-len ,jump-to))

(defmacro type-cast (operand-stack constructor)
  `(stack-push ,operand-stack (funcall ,constructor (stack-pop ,operand-stack))))

(defmacro iadd (operand-stack)
  `(arith ,operand-stack #'integer-add))

(defmacro iaload (operand-stack)
  `(array-load ,operand-stack))

(defmacro iand (operand-stack)
  `(arith ,operand-stack #'integer-and))

(defmacro iastore (operand-stack)
  `(array-store ,operand-stack '(integer)))

(declaim (inline idiv))
(defun idiv (operand-stack)
  (let ((r (arith operand-stack #'integer-div)))
    (when (symbolp r)
      (throw-exception r))
    r))

(defmacro if-cmp (operand-stack predicate jump-to)
  `(if (funcall ,predicate 
		(stack-pop ,operand-stack)
		(stack-pop ,operand-stack))
       ,jump-to
       nil))

(defmacro if-cmp-with (operand-stack value predicate jump-to)
  `(if (funcall ,predicate 
		(stack-pop ,operand-stack)
		,value)
       ,jump-to
       nil))

(declaim (inline iinc))
(defun iinc (locals index const)
  (let ((v (aref locals index)))
    (setf (aref locals index) (+ v const))))

(defmacro imul (operand-stack)
  `(arith ,operand-stack #'integer-mul))

(defmacro ineg (operand-stack)
  `(stack-push ,operand-stack (integer-neg (stack-pop ,operand-stack))))

(defmacro instanceof (operand-stack constant-pool index)
  `(stack-push ,operand-stack 
	       (is-instance-of (constant-pool-string-at ,constant-pool ,index)
			       (stack-pop ,operand-stack))))

(defmacro invokedynamic (operand-stack constant-pool index)
  `(invoke-dynamic (constant-pool-string-at ,constant-pool ,index)
		   ,operand-stack))

(defmacro invokeinterface (operand-stack constant-pool index count)
  `(invoke-interface (constant-pool-string-at ,constant-pool ,index)
		     ,operand-stack ,count))

(defmacro invokespecial (operand-stack constant-pool index)
  `(invoke-special (constant-pool-string-at ,constant-pool ,index)
		   ,operand-stack))

(defmacro invokestatic (operand-stack constant-pool index)
  `(invoke-static (constant-pool-string-at ,constant-pool ,index)
		  ,operand-stack))

(defmacro invokevirtual (operand-stack constant-pool index)
  `(invoke-virtual (constant-pool-string-at ,constant-pool ,index)
		  ,operand-stack))

(defmacro ior (operand-stack)
  `(arith ,operand-stack #'integer-or))

(declaim (inline irem))
(defun irem (operand-stack)
  (let ((r (arith operand-stack #'integer-rem)))
    (when (symbolp r)
      (throw-exception r))
    r))

(defmacro ishl (operand-stack)
  `(arith ,operand-stack #'integer-shift-left))

(defmacro ishr (operand-stack)
  `(arith ,operand-stack #'integer-shift-right))

(defmacro isub (operand-stack)
  `(arith ,operand-stack #'integer-sub))

(defmacro iushr (operand-stack)
  `(arith ,operand-stack #'integer-logical-shift-right))

(defmacro ixor (operand-stack)
  `(arith ,operand-stack #'integer-xor))

(declaim (inline jsr))
(defun jsr (operand-stack byte-code 
	    byte-code-len jump-to
	    next-opc)
  (let ((pc (find-pc 'jsr byte-code byte-code-len jump-to)))
    (stack-push operand-stack (make-return-address (opcode-offset next-opc)))
    pc))

(defmacro ladd (operand-stack)
  `(arith ,operand-stack #'long-add))

(defmacro laload (operand-stack)
  `(array-load ,operand-stack))

(defmacro land (operand-stack)
  `(arith ,operand-stack #'long-and))

(defmacro lastore (operand-stack)
  `(array-store ,operand-stack '(long)))

(defmacro lcmp (operand-stack)
  `(arith ,operand-stack #'long-compare))

(defmacro ldc (operand-stack constant-pool index)
  `(format t "(ldc ~a ~a ~a) not implemented!~%"
	   ,operand-stack ,constant-pool ,index))

(defmacro ldc2 (operand-stack constant-pool index)
  `(format t "(ldc2 ~a ~a ~a) not implemented!~%"
	   ,operand-stack ,constant-pool ,index))

(defmacro ldiv (operand-stack)
  `(arith ,operand-stack #'long-div))

(defmacro lmul (operand-stack)
  `(arith ,operand-stack #'long-mul))

(defmacro lneg (operand-stack)
  `(stack-push ,operand-stack (long-neg (stack-pop ,operand-stack))))

(declaim (inline lookupswitch))
(defun lookupswitch (key byte-code 
		     byte-code-len 
		     current-offset operand)
  (let ((offset (car operand))
	(match-offsets (cdr operand)))
    (loop for i from 0 to (1- (length match-offsets))
       do (let ((e (aref match-offsets i)))
	    (when (= key (car e))
	      (setf offset (cdr e))
	      (return))))
    (find-pc 'lookupswitch byte-code byte-code-len 
	     (+ current-offset offset))))

(defmacro lor (operand-stack)
  `(arith ,operand-stack #'long-or))

(defmacro lrem (operand-stack)
  `(arith ,operand-stack #'long-rem))

(defmacro lshl (operand-stack)
  `(arith ,operand-stack #'long-shift-left))

(defmacro lshr (operand-stack)
  `(arith ,operand-stack #'long-shift-right))

(defmacro lsub (operand-stack)
  `(arith ,operand-stack #'long-sub))

(defmacro lushr (operand-stack)
  `(arith ,operand-stack #'long-logical-shift-right))

(defmacro lxor (operand-stack)
  `(arith ,operand-stack #'long-xor))

(declaim (inline monitorenter))
(defun monitorenter (operand-stack)
  (format t "(monitorenter ~a) not fully implemented!~%" operand-stack)
  (stack-pop operand-stack))

(declaim (inline monitorexit))
(defun monitorexit (operand-stack)
  (format t "(monitorexit ~a) not fully implemented!~%" operand-stack)
  (stack-pop operand-stack))

(declaim (inline multianewarray))
(defun multianewarray (operand-stack dimensions-count
		       constant-pool pool-index)
  (when (< dimensions-count 0)
    (throw-exception 'NegativeArraySizeException))
  (let ((dimensions nil))
    (loop for i from 0 to (1- dimensions-count)
       do (setf dimensions (cons (stack-pop operand-stack) 
				 dimensions)))
    (stack-push operand-stack (make-typed-array 
			       (reverse dimensions)
			       (constant-pool-string-at 
				constant-pool pool-index)))))

(declaim (inline new))
(defun new (operand-stack)
  (format t "(new ~a) not implemented!~%" operand-stack))

(declaim (inline newarray))
(defun newarray (operand-stack type)
  (let ((c (stack-pop operand-stack)))
    (when (< c 0)
      (throw-exception 'NegativeArraySizeException))
    (stack-push operand-stack (make-typed-array c (atype-to-symbol type)))))

(declaim (inline pop2))
(defun pop2 (operand-stack)
  (let ((v1 (stack-pop operand-stack)))
    (when (is-category-1 v1)
      (stack-pop operand-stack))))

(declaim (inline putfield))
(defun putfield (operand-stack constant-pool pool-index)
  (let ((value (stack-pop operand-stack))
	(objref (stack-pop operand-stack))
	(field-name (constant-pool-string-at constant-pool pool-index)))
    (format t "~a.~a = ~a~%" objref field-name value)
    (format t "putfield not fully implemented!~%")))

(declaim (inline putstatic))
(defun putstatic (operand-stack constant-pool pool-index)
  (let ((value (stack-pop operand-stack))
	(field-name (constant-pool-string-at constant-pool pool-index)))
    (format t "~a = ~a~%" field-name value)
    (format t "putstatic not fully implemented!~%")))

(defmacro ret (locals index)
  `(return-address-value (aref ,locals ,index)))

(defmacro saload (operand-stack)
  `(array-load ,operand-stack))

(defmacro sastore (operand-stack)
  `(array-store ,operand-stack '(short)))

(declaim (inline tableswitch))
(defun tableswitch (index ts 
		    byte-code byte-code-len 
		    current-offset)
  (let ((default (table-switch-default ts))
	(low (table-switch-low ts))
	(hi (table-switch-hi ts)))
    (let ((offset (if (or (< default low)
			  (> default hi))
		      default
		      (aref (table-switch-jump-offsets ts)
			    (- index low)))))
      (find-pc 'tableswitch byte-code byte-code-len
	       (+ offset current-offset)))))
	
	  