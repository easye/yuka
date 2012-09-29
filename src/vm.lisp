(load "packages.lisp")

(in-package :yuka)

;; Required by the class-loader.
(load "trivial-utf-8.lisp")
(load "util.lisp")
(load "opc.lisp")
(load "access-flags.lisp")
(load "constant-pool.lisp")
(load "attribute-info.lisp")
(load "field-method-info.lisp")
(load "klass.lisp")
(load "klass-loader.lisp")

;; Required by the virtual machine.
(load "opc-impl.lisp")
(load "frame.lisp")

(defun execute-method (method klass)
  (frame-run (make-frame-from-code (method-code-attribute method)
                                   klass)))

(defun run-method (klass method-name)
  (let ((method (klass-find-method-by-name klass method-name)))
    (when (null method)
      (error "Error: Method  `~a` not found in class ~a.~%" 
	     method-name (klass-name klass)))
    (execute-method method klass)))

(defun run-main (klass)
  (run-method klass "main"))
	