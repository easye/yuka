(in-package :yuka)

(defmacro throw-exception (exception)
  `(error "~a" ,exception))

(defmacro check-not-null (obj)
  `(when (null ,obj)
     (throw-exception 'NullPointerException)))

(defmacro check-bounds (array index)
  `(when (>= ,index (length ,array))
     (throw-exception 'ArrayIndexOutOfBoundsException)))

(defmacro check-array-type (array value)
  `(format t "(check-array-type ~a ~a) not implemented!~%"
           ,array ,value))  

(defmacro check-for-neg-array-size (size)
  `(when (< ,size 0)
     (throw-exception 'NegativeArraySizeException)))

(defmacro check-ref-type (obj types)
  `(format t "(check-ref-type ~a ~a) not implemented!~%" 
           ,obj ,types))

(defmacro istore (locals operand-stack index)
  `(setf (aref ,locals ,index) (stack-pop ,operand-stack)))

(defun aaload (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack)))
    (check-not-null array)
    (check-bounds array index)
    (stack-push operand-stack (aref array index))))

(defun aastore (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack))
        (value (stack-pop operand-stack)))
    (check-not-null array)
    (check-bounds array index)
    (check-array-type array value)
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
    (check-ref-type ref '(return-address reference))
    (setf (aref locals index) ref)))