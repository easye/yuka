(in-package :yuka)

(defconstant +magic+ #xCAFEBABE)
(defconstant +max-code-len+ 65536)

(defun read-u2 (stream)
  (let ((i 0))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    i))

(defun read-u4 (stream)
  (let ((i 0))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    (setf i (ash i 8))
    (setf i (logior i (logand (read-byte stream) #xff)))
    i))

(defun read-array (stream count reader-fn)
  (let ((data (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref data i) (funcall reader-fn stream)))
    data))

(defun read-array-with-user-data (stream count user-data reader-fn)
  (let ((data (make-array count)))
    (loop for i from 0 to (1- count)
       do (setf (aref data i) (funcall reader-fn stream user-data)))
    data))

(defstruct iterator
  (sequence nil)
  (state nil)) ;; length, current index etc.

(defvar *eos* :eos) ;; end-of-sequence marker.

(defmacro eos-p (obj)
  `(eq ,*eos* ,obj))

(defstruct array-state
  (len 0 :type integer)
  (index 0 :type integer))

(defun make-array-iterator (array)
  (make-iterator :sequence array 
		 :state (make-array-state :len (length array))))

(defun iterator-next (self)
  (let* ((state (iterator-state self))
	 (i (array-state-index state)))
    (cond ((>= i (array-state-len state))
	   *eos*)
	  (t (let ((v (aref (iterator-sequence self) i)))
	       (setf (array-state-index state) (1+ i))
	       v)))))

(defstruct bounded-stack
  (elements nil :type simple-array)
  (index 0 :type integer))

(defmacro make-stack (max-depth)
  `(make-bounded-stack :elements (make-array ,max-depth)))

(defun stack-push (self e)
  (let ((elements (bounded-stack-elements self))
	(index (bounded-stack-index self)))
    (when (>= index (length elements))
      (error "stack overflow."))
    (when (< index 0)
      (setf index 0))
    (setf (aref elements index) e)
    (setf (bounded-stack-index self) (1+ index))))

(defun stack-pop (self)
  (let ((elements (bounded-stack-elements self))
	(index (1- (bounded-stack-index self))))
    (when (< index 0)
      (error "stack underflow."))
    (setf (bounded-stack-index self) index)
    (aref elements index)))

(defun stack-top (self)
  (let ((e (stack-pop self))
        (i (bounded-stack-index self)))
    (setf (bounded-stack-index self) (1+ i))
    e))

(defmacro stack-reset (self)
  `(setf (bounded-stack-index ,self) 0))

(defmacro make-typed-array (count type-name)
  `(cons (make-array ,count) ,type-name))

(defmacro typed-array-elements (self)
  `(car ,self))

(defmacro typed-array-type-name (self)
  `(cdr ,self))
