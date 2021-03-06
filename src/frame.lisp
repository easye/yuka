;;;; Copyright (c) 2012 Vijay Mathew Pandyalakal <vijay.the.lisper@gmail.com>

;;;; This file is part of yuka.

;;;; yuka is free software; you can redistribute it and/or modify it under
;;;; the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 3 of the License, or
;;;; (at your option) any later version.

;;;; yuka is distributed in the hope that it will be useful,
;;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;; GNU General Public License for more details.

;;;; You should have received a copy of the GNU General Public License
;;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :yuka)

;;; The data structure and related functions required for
;;; executing the bytecode of a method.

(defstruct frame
  (code nil)
  (klass nil)
  (locals nil :type simple-array)
  (operand-stack nil)
  (has-unhandled-exception nil)
  (return-value nil))

(defun make-frame-from-code (code klass)
  (make-frame :code code
              :klass klass
	      :locals (make-array (code-attribute-max-locals code))
	      :operand-stack (make-stack (code-attribute-max-stack code))))

(defun frame-run (self)
  (let* ((pc 0) (new-pc nil) 
         (code (frame-code self))
         (bcode (code-attribute-code code)) 
         (len (length bcode))
         (cp (klass-constant-pool (frame-klass self)))
         (locals (frame-locals self))
         (operand-stack (frame-operand-stack self))
         (return-from-frame nil))
    (format t "~a~%" code)
    (loop (when (or (>= pc len) return-from-frame)
            (return))
       (let ((opc (aref bcode pc)))
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
            (monitorexit operand-stack)
            (setf return-from-frame t))
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
           ((athrow)
            (setf new-pc (athrow operand-stack 
                             (code-attribute-exception-table code)
                             cp))                             
            (when (< new-pc 0)
              (monitorexit operand-stack)
              (setf (frame-has-unhandled-exception self) t)
              (setf return-from-frame t)))
           ((baload)
            (baload operand-stack))
           ((bastore)
            (bastore operand-stack))
           ((bipush)
            (stack-push operand-stack (opcode-operands opc)))
           ((caload)
            (caload operand-stack))
           ((castore)
            (castore operand-stack))
           ((checkcast)
            (checkcast operand-stack (opcode-operands opc) cp))
           ((d2f)
            (d2f operand-stack))
           ((d2i)
            (d2i operand-stack))
	   ((d2l)
	    (d2l operand-stack))
	   ((dadd)
	    (dadd operand-stack))
	   ((daload)
	    (daload operand-stack))
	   ((dastore)
	    (dastore operand-stack))
	   ((dcmpg)
	    (dcmp operand-stack nil))
	   ((dcmpl)
	    (dcmp operand-stack t))
	   ((dconst_0)
	    (stack-push operand-stack +float-zero+))
	   ((dconst_1)
	    (stack-push operand-stack +float-one+))
	   ((ddiv)
	    (ddiv operand-stack))
	   ((dload fload iload lload)
	    (loadv locals operand-stack (opcode-operands opc)))
	   ((dload_0 fload_0 iload_0 lload_0)
	    (loadv locals operand-stack 0))
	   ((dload_1 fload_1 iload_1 lload_1)
	    (loadv locals operand-stack 1))
	   ((dload_2 fload_2 iload_2 lload_2)
	    (loadv locals operand-stack 2))
	   ((dload_3 fload_3 iload_3 lload_3)
	    (loadv locals operand-stack 3))
	   ((dmul)
	    (dmul operand-stack))
	   ((dneg)
	    (dneg operand-stack))
	   ((drem)
	    (drem operand-stack))
	   ((dreturn freturn ireturn lreturn)
	    (monitorexit operand-stack)
	    (setf (frame-return-value self) (stack-pop operand-stack))
	    (setf return-from-frame t))
	   ((dstore fstore istore lstore)
	    (storev locals operand-stack (opcode-operands opc)))
           ((dstore_0 fstore_0 istore_0 lstore_0)
            (storev locals operand-stack 0))
           ((dstore_1 fstore_1 istore_1 lstore_1)
            (storev locals operand-stack 1))
           ((dstore_2 fstore_2 istore_2 lstore_2)
            (storev locals operand-stack 2))
           ((dstore_3 fstore_3 istore_3 lstore_3)
            (storev locals operand-stack 3))
	   ((dsub)
	    (dsub operand-stack))
	   ((dup)
	    (stack-dup operand-stack))
	   ((dup_x1)
	    (stack-dup-x1 operand-stack))
	   ((dup_x2)
	    (stack-dup-x2 operand-stack))
	   ((dup2)
	    (stack-dup2 operand-stack))
	   ((dup2_x1)
	    (stack-dup2-x1 operand-stack))
	   ((dup2_x2)
	    (stack-dup2-x2 operand-stack))
	   ((f2i)
	    (f2i operand-stack))
	   ((f2l)
	    (f2l operand-stack))
	   ((fadd)
	    (fadd operand-stack))
	   ((faload)
	    (faload operand-stack))
	   ((fastore)
	    (fastore operand-stack))
	   ((fcmpg)
	    (dcmp operand-stack nil))
	   ((fcmpl)
	    (dcmp operand-stack t))
	   ((fconst_0)
	    (stack-push operand-stack +float-zero+))
	   ((fconst_1)
	    (stack-push operand-stack +float-one+))
	   ((fconst_2)
	    (stack-push operand-stack +float-two+))
	   ((fdiv)
	    (fdiv operand-stack))
	   ((fmul)
	    (fmul operand-stack))
	   ((fneg)
	    (fneg operand-stack))
	   ((frem)
	    (frem operand-stack))
	   ((fsub)
	    (fsub operand-stack))
	   ((getfield)
	    (getfield operand-stack cp (opcode-operands opc)))
	   ((getstatic)
	    (getstatic operand-stack cp (opcode-operands opc)))
           ((goto goto_w)
            (setf new-pc (goto bcode len (opcode-operands opc))))
	   ((i2b)
	    (type-cast operand-stack #'make-byte))
	   ((i2c)
	    (type-cast operand-stack #'make-char))
	   ((i2d)
	    (type-cast operand-stack #'integer-to-double))
	   ((i2f)
	    (type-cast operand-stack #'integer-to-float))
	   ((i2l)
	    (type-cast operand-stack #'make-long))
	   ((i2s)
	    (type-cast operand-stack #'make-short))
	   ((iadd)
	    (iadd operand-stack))
	   ((iaload)
	    (iaload operand-stack))
	   ((iand)
	    (iand operand-stack))
	   ((iastore)
	    (iastore operand-stack))
	   ((iconst_m1)
	    (stack-push operand-stack -1))
	   ((iconst_0)
	    (stack-push operand-stack 0))
	   ((iconst_1)
	    (stack-push operand-stack 1))
	   ((iconst_2)
	    (stack-push operand-stack 2))
	   ((iconst_3)
	    (stack-push operand-stack 3))
	   ((iconst_4)
	    (stack-push operand-stack 4))
	   ((iconst_5)
	    (stack-push operand-stack 5))
	   ((idiv)
	    (idiv operand-stack))
	   ((if_acmpeq)
	    (setf new-pc (if-cmp operand-stack #'eq (opcode-operands opc))))
	   ((if_acmpne)
	    (setf new-pc (if-cmp operand-stack #'neq (opcode-operands opc))))
	   ((if_icmpeq)
	    (setf new-pc (if-cmp operand-stack #'= (opcode-operands opc))))
	   ((if_icmpne)
	    (setf new-pc (if-cmp operand-stack #'n= (opcode-operands opc))))
	   ((if_icmplt)
	    (setf new-pc (if-cmp operand-stack #'< (opcode-operands opc))))
	   ((if_icmpge)
	    (setf new-pc (if-cmp operand-stack #'>= (opcode-operands opc))))
	   ((if_icmpgt)
	    (setf new-pc (if-cmp operand-stack #'> (opcode-operands opc))))
	   ((if_icmple)
	    (setf new-pc (if-cmp operand-stack #'<= (opcode-operands opc))))
	   ((ifeq)
	    (setf new-pc (if-cmp-with operand-stack 0 #'= (opcode-operands opc))))
	   ((ifne)
	    (setf new-pc (if-cmp-with operand-stack 0 #'n= (opcode-operands opc))))
	   ((iflt)
	    (setf new-pc (if-cmp-with operand-stack 0 #'< (opcode-operands opc))))
	   ((ifge)
	    (setf new-pc (if-cmp-with operand-stack 0 #'>= (opcode-operands opc))))
	   ((ifgt)
	    (setf new-pc (if-cmp-with operand-stack 0 #'> (opcode-operands opc))))
	   ((ifle)
	    (setf new-pc (if-cmp-with operand-stack 0 #'<= (opcode-operands opc))))
	   ((ifnonnull)
	    (setf new-pc (if-cmp-with operand-stack nil #'neq (opcode-operands opc))))
	   ((ifnull)
	    (setf new-pc (if-cmp-with operand-stack nil #'eq (opcode-operands opc))))
	   ((iinc)
	    (let ((operands (opcode-operands opc)))
	      (iinc locals (car operands) (cdr operands))))
	   ((imul)
	    (imul operand-stack))
	   ((ineg)
	    (ineg operand-stack))
	   ((instanceof)
	    (instanceof operand-stack cp (opcode-operands opc)))
	   ((invokedynamic)
	    (invokedynamic operand-stack cp (car (opcode-operands opc))))
	   ((invokeinterface)
	    (invokeinterface operand-stack cp (car (opcode-operands opc))
			     (cadr (opcode-operands opc))))
	   ((invokespecial)
	    (invokespecial operand-stack cp (opcode-operands opc)))
	   ((invokestatic)
	    (invokestatic operand-stack cp (opcode-operands opc)))
	   ((ior)
	    (ior operand-stack))
	   ((irem)
	    (irem operand-stack))
	   ((ishl)
	    (ishl operand-stack))
	   ((ishr)
	    (ishr operand-stack))
	   ((isub)
	    (isub operand-stack))
	   ((iushr)
	    (iushr operand-stack))
	   ((ixor)
	    (ixor operand-stack))
	   ((jsr jsr_w)
	    (setf new-pc (jsr operand-stack bcode len (opcode-operands opc)
			      (aref bcode (1+ pc)))))
	   ((l2d)
	    (type-cast operand-stack #'long-to-double))
	   ((l2f)
	    (type-cast operand-stack #'long-to-float))
	   ((l2i)
	    (type-cast operand-stack #'long-to-integer))
	   ((ladd)
	    (ladd operand-stack))
	   ((laload)
	    (laload operand-stack))
	   ((land)
	    (land operand-stack))
	   ((lastore)
	    (lastore operand-stack))
	   ((lcmp)
	    (lcmp operand-stack))
	   ((lconst_0)
	    (stack-push operand-stack 0))
	   ((lconst_1)
	    (stack-push operand-stack 1))
	   ((ldc ldc_w)
	    (ldc operand-stack cp (opcode-operands opc)))
	   ((ldc2_w)
	    (ldc2 operand-stack cp (opcode-operands opc)))
	   ((ldiv)
	    (ldiv operand-stack))
	   ((lmul)
	    (lmul operand-stack))
	   ((lneg)
	    (lneg operand-stack))
	   ((lookupswitch)
	    (setf new-pc (lookupswitch (stack-pop operand-stack) 
				       bcode len (opcode-offset opc) 
				       (opcode-operands opc))))
	   ((lor)
	    (lor operand-stack))
	   ((lrem)
	    (lrem operand-stack))
	   ((lshl)
	    (lshl operand-stack))
	   ((lshr)
	    (lshr operand-stack))
	   ((lsub)
	    (lsub operand-stack))
	   ((lushr)
	    (lushr operand-stack))
	   ((lxor)
	    (lxor operand-stack))
	   ((monitorenter)
	    (monitorenter operand-stack))
	   ((monitorexit)
	    (monitorexit operand-stack))
	   ((multianewarray)
	    (let ((operands (opcode-operands opc)))
	      (multianewarray operand-stack 
			      (cdr operands)
			      cp
			      (car operands))))
	   ((new)
	    (new operand-stack))
	   ((newarray)
	    (newarray operand-stack (opcode-operands opc)))
	   ((nop) nil)
	   ((pop)
	    (stack-pop operand-stack))
	   ((pop2)
	    (pop2 operand-stack))
	   ((putfield)
	    (putfield operand-stack cp (opcode-operands opc)))
	   ((putstatic)
	    (putstatic operand-stack cp (opcode-operands opc)))
	   ((ret)
	    (setf new-pc (ret locals (opcode-operands opc))))
           ((return)
            (monitorexit operand-stack)
            (setf return-from-frame t))
	   ((saload)
	    (saload operand-stack))
	   ((sastore)
	    (sastore operand-stack))
	   ((sipush)
	    (stack-push operand-stack (opcode-operands opc)))
	   ((swap)
	    (stack-swap operand-stack))
	   ((tableswitch)
	    (tableswitch (stack-pop operand-stack) (opcode-operands opc)
			 bcode len
			 (opcode-offset opc)))
	   ((wide)
	    (error "wide instruction is not implemented!~%")))
         (cond (new-pc
                (setf pc new-pc)
                (setf new-pc nil))
               (t (setf pc (1+ pc)))))))
  self)
