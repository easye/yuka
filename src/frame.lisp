(in-package :yuka)

(defstruct frame
  (code nil)
  (klass nil)
  (locals nil :type simple-array)
  (operand-stack nil))

(defun make-frame-from-code (code klass)
  (make-frame :code (code-attribute-code code)
              :klass klass
	      :locals (make-array (code-attribute-max-locals code))
	      :operand-stack (make-stack (code-attribute-max-stack code))))

(defun monitor-exit ()
  (format t "(monitor-exit) not implemented!~%"))

(defun frame-run (self)
  (let ((iter (make-array-iterator (frame-code self)))
        (cp (klass-constant-pool (frame-klass self)))
	(locals (frame-locals self))
	(operand-stack (frame-operand-stack self)))
    (loop (let ((opc (iterator-next iter)))
	    (when (eos-p opc)
	      (return))
	    (case (opcode-symbol opc)
              ((aaload)
               (aaload operand-stack))
              ((aastore)
               (aastore operand-stack))
              ((aconst_null)
               (stack-push operand-stack nil))
              ((aload)
               (aload locals operand-stack (opcode-operands opc)))
              ((aload_0)
               (aload locals operand-stack 0))
              ((aload_1)
               (aload locals operand-stack 1))
              ((aload_2)
               (aload locals operand-stack 2))
              ((aload_3)
               (aload locals operand-stack 3))
              ((anewarray)
               (anewarray operand-stack cp (opcode-operands opc)))
              ((areturn)
               (monitor-exit)
               (return (stack-pop operand-stack)))
              ((arraylength)
               (arraylength operand-stack))
              ((astore)
               (astore locals operand-stack (opcode-operands opc)))
              ((astore_0)
               (astore locals operand-stack 0))
              ((astore_1)
               (astore locals operand-stack 1))
              ((astore_2)
               (astore locals operand-stack 2))
              ((astore_3)
               (astore locals operand-stack 3))
	      ((bipush)
	       (stack-push operand-stack (opcode-operands opc)))
	      ((istore_0)
	       (istore locals operand-stack 0))
	      ((istore_1)
	       (istore locals operand-stack 1))
	      ((istore_2)
	       (istore locals operand-stack 2))
	      ((istore_3)
	       (istore locals operand-stack 3))
	      ((return)
               (monitor-exit)
	       (return :void))))))
  self)

