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

(defmacro check-array (array index types)
  `(check-not-null ,array)
  `(check-bounds ,array ,index)
  `(check-array-type ,array ,types))

(defmacro istore (locals operand-stack index)
  `(setf (aref ,locals ,index) (stack-pop ,operand-stack)))

(defun aaload (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack)))
    (check-array array index '(reference))
    (stack-push operand-stack (aref array index))))

(defun aastore (operand-stack)
  (let* ((a (stack-pop operand-stack))
         (array (typed-array-elements a))
         (index (stack-pop operand-stack))
        (value (stack-pop operand-stack)))
    (check-array array index '(reference))
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

(defun array-load (operand-stack types)
    (let ((array (stack-pop operand-stack))
        (index (stack-pop operand-stack)))
      (check-array array index types)
      (stack-push operand-stack (aref array index))))

(defun array-store (operand-stack types)
  (let ((array (stack-pop operand-stack))
        (index (stack-pop operand-stack))
        (value (stack-pop operand-stack)))
    (check-array array index types)
    (check-obj-type value types)
    (setf (aref array index) value)))

(defmacro baload (operand-stack)
  `(array-load ,operand-stack '(byte boolean)))

(defmacro bastore (operand-stack)
  `(array-store ,operand-stack '(bye boolean)))

(defmacro caload (operand-stack)
  `(array-load ,operand-stack '(char)))

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