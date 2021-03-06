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

;;; Representation of opcodes.

(defparameter *opcode-symbols* (make-array 255))

(defun init-opcode-symbols-table ()
  (setf (aref *opcode-symbols* 50) 'aaload)
  (setf (aref *opcode-symbols* 83) 'aastore)
  (setf (aref *opcode-symbols* 1) 'aconst_null)
  (setf (aref *opcode-symbols* 25) 'aload)
  (setf (aref *opcode-symbols* 42) 'aload_0)
  (setf (aref *opcode-symbols* 43) 'aload_1)
  (setf (aref *opcode-symbols* 44) 'aload_2)
  (setf (aref *opcode-symbols* 45) 'aload_3)
  (setf (aref *opcode-symbols* 189) 'anewarray)
  (setf (aref *opcode-symbols* 176) 'areturn)
  (setf (aref *opcode-symbols* 190) 'arraylength)
  (setf (aref *opcode-symbols* 58) 'astore)
  (setf (aref *opcode-symbols* 75) 'astore_0)
  (setf (aref *opcode-symbols* 76) 'astore_1)
  (setf (aref *opcode-symbols* 77) 'astore_2)
  (setf (aref *opcode-symbols* 78) 'astore_3)
  (setf (aref *opcode-symbols* 191) 'athrow)
  (setf (aref *opcode-symbols* 51) 'baload)
  (setf (aref *opcode-symbols* 84) 'bastore)
  (setf (aref *opcode-symbols* 16) 'bipush)
  (setf (aref *opcode-symbols* 52) 'caload)
  (setf (aref *opcode-symbols* 85) 'castore)
  (setf (aref *opcode-symbols* 192) 'checkcast)
  (setf (aref *opcode-symbols* 144) 'd2f)
  (setf (aref *opcode-symbols* 142) 'd2i)
  (setf (aref *opcode-symbols* 143) 'd2l)
  (setf (aref *opcode-symbols* 99) 'dadd)
  (setf (aref *opcode-symbols* 49) 'daload)
  (setf (aref *opcode-symbols* 82) 'dastore)
  (setf (aref *opcode-symbols* 152) 'dcmpg)
  (setf (aref *opcode-symbols* 151) 'dcmpl)
  (setf (aref *opcode-symbols* 14) 'dconst_0)
  (setf (aref *opcode-symbols* 15) 'dconst_1)
  (setf (aref *opcode-symbols* 111) 'ddiv)
  (setf (aref *opcode-symbols* 24) 'dload)
  (setf (aref *opcode-symbols* 38) 'dload_0)
  (setf (aref *opcode-symbols* 39) 'dload_1)
  (setf (aref *opcode-symbols* 40) 'dload_2)
  (setf (aref *opcode-symbols* 41) 'dload_3)
  (setf (aref *opcode-symbols* 107) 'dmul)
  (setf (aref *opcode-symbols* 119) 'dneg)
  (setf (aref *opcode-symbols* 115) 'drem)
  (setf (aref *opcode-symbols* 175) 'dreturn)
  (setf (aref *opcode-symbols* 57) 'dstore)
  (setf (aref *opcode-symbols* 71) 'dstore_0)
  (setf (aref *opcode-symbols* 72) 'dstore_1)
  (setf (aref *opcode-symbols* 73) 'dstore_2)
  (setf (aref *opcode-symbols* 74) 'dstore_3)
  (setf (aref *opcode-symbols* 103) 'dsub)
  (setf (aref *opcode-symbols* 89) 'dup)
  (setf (aref *opcode-symbols* 90) 'dup_x1)
  (setf (aref *opcode-symbols* 91) 'dup_x2)
  (setf (aref *opcode-symbols* 92) 'dup2)
  (setf (aref *opcode-symbols* 93) 'dup2_x1)
  (setf (aref *opcode-symbols* 94) 'dup2_x2)
  (setf (aref *opcode-symbols* 141) 'f2d)
  (setf (aref *opcode-symbols* 139) 'f2i)
  (setf (aref *opcode-symbols* 140) 'f2l)
  (setf (aref *opcode-symbols* 98) 'fadd)
  (setf (aref *opcode-symbols* 48) 'faload)
  (setf (aref *opcode-symbols* 81) 'fastore)
  (setf (aref *opcode-symbols* 150) 'fcmpg)
  (setf (aref *opcode-symbols* 149) 'fcmpl)
  (setf (aref *opcode-symbols* 11) 'fconst_0)
  (setf (aref *opcode-symbols* 12) 'fconst_1)
  (setf (aref *opcode-symbols* 13) 'fconst_2)
  (setf (aref *opcode-symbols* 110) 'fdiv)
  (setf (aref *opcode-symbols* 23) 'fload)
  (setf (aref *opcode-symbols* 34) 'fload_0)
  (setf (aref *opcode-symbols* 35) 'fload_1)
  (setf (aref *opcode-symbols* 36) 'fload_2)
  (setf (aref *opcode-symbols* 37) 'fload_3)
  (setf (aref *opcode-symbols* 106) 'fmul)
  (setf (aref *opcode-symbols* 118) 'fneg)
  (setf (aref *opcode-symbols* 114) 'frem)
  (setf (aref *opcode-symbols* 174) 'freturn)
  (setf (aref *opcode-symbols* 56) 'fstore)
  (setf (aref *opcode-symbols* 67) 'fstore_0)
  (setf (aref *opcode-symbols* 68) 'fstore_1)
  (setf (aref *opcode-symbols* 69) 'fstore_2)
  (setf (aref *opcode-symbols* 70) 'fstore_3)
  (setf (aref *opcode-symbols* 102) 'fsub)
  (setf (aref *opcode-symbols* 180) 'getfield)
  (setf (aref *opcode-symbols* 178) 'getstatic)
  (setf (aref *opcode-symbols* 167) 'goto)
  (setf (aref *opcode-symbols* 200) 'goto_w)
  (setf (aref *opcode-symbols* 145) 'i2b)
  (setf (aref *opcode-symbols* 146) 'i2c)
  (setf (aref *opcode-symbols* 135) 'i2d)
  (setf (aref *opcode-symbols* 134) 'i2f)
  (setf (aref *opcode-symbols* 133) 'i2l)
  (setf (aref *opcode-symbols* 147) 'i2s)
  (setf (aref *opcode-symbols* 96) 'iadd)
  (setf (aref *opcode-symbols* 46) 'iaload)
  (setf (aref *opcode-symbols* 126) 'iand)
  (setf (aref *opcode-symbols* 79) 'iastore)
  (setf (aref *opcode-symbols* 2) 'iconst_m1)
  (setf (aref *opcode-symbols* 3) 'iconst_0)
  (setf (aref *opcode-symbols* 4) 'iconst_1)
  (setf (aref *opcode-symbols* 5) 'iconst_2)
  (setf (aref *opcode-symbols* 6) 'iconst_3)
  (setf (aref *opcode-symbols* 7) 'iconst_4)
  (setf (aref *opcode-symbols* 8) 'iconst_5)
  (setf (aref *opcode-symbols* 108) 'idiv)
  (setf (aref *opcode-symbols* 165) 'if_acmpeq)
  (setf (aref *opcode-symbols* 166) 'if_acmpne)
  (setf (aref *opcode-symbols* 159) 'if_icmpeq)
  (setf (aref *opcode-symbols* 160) 'if_icmpne)
  (setf (aref *opcode-symbols* 161) 'if_icmplt)
  (setf (aref *opcode-symbols* 162) 'if_icmpge)
  (setf (aref *opcode-symbols* 163) 'if_icmpgt)
  (setf (aref *opcode-symbols* 164) 'if_icmple)
  (setf (aref *opcode-symbols* 153) 'ifeq)
  (setf (aref *opcode-symbols* 154) 'ifne)
  (setf (aref *opcode-symbols* 155) 'iflt)
  (setf (aref *opcode-symbols* 156) 'ifge)
  (setf (aref *opcode-symbols* 157) 'ifgt)
  (setf (aref *opcode-symbols* 158) 'ifle)
  (setf (aref *opcode-symbols* 199) 'ifnonnull)
  (setf (aref *opcode-symbols* 198) 'ifnull)
  (setf (aref *opcode-symbols* 132) 'iinc)
  (setf (aref *opcode-symbols* 21) 'iload)
  (setf (aref *opcode-symbols* 26) 'iload_0)
  (setf (aref *opcode-symbols* 27) 'iload_1)
  (setf (aref *opcode-symbols* 28) 'iload_2)
  (setf (aref *opcode-symbols* 29) 'iload_3)
  (setf (aref *opcode-symbols* 104) 'imul)
  (setf (aref *opcode-symbols* 116) 'ineg)
  (setf (aref *opcode-symbols* 193) 'instanceof)
  (setf (aref *opcode-symbols* 186) 'invokedynamic)
  (setf (aref *opcode-symbols* 185) 'invokeinterface)
  (setf (aref *opcode-symbols* 183) 'invokespecial)
  (setf (aref *opcode-symbols* 184) 'invokestatic)
  (setf (aref *opcode-symbols* 182) 'invokevirtual)
  (setf (aref *opcode-symbols* 128) 'ior)
  (setf (aref *opcode-symbols* 112) 'irem)
  (setf (aref *opcode-symbols* 172) 'ireturn)
  (setf (aref *opcode-symbols* 120) 'ishl)
  (setf (aref *opcode-symbols* 122) 'ishr)
  (setf (aref *opcode-symbols* 54) 'istore)
  (setf (aref *opcode-symbols* 59) 'istore_0)
  (setf (aref *opcode-symbols* 60) 'istore_1)
  (setf (aref *opcode-symbols* 61) 'istore_2)
  (setf (aref *opcode-symbols* 62) 'istore_3)
  (setf (aref *opcode-symbols* 100) 'isub)
  (setf (aref *opcode-symbols* 124) 'iushr)
  (setf (aref *opcode-symbols* 130) 'ixor)
  (setf (aref *opcode-symbols* 168) 'jsr)
  (setf (aref *opcode-symbols* 201) 'jsr_w)
  (setf (aref *opcode-symbols* 138) 'l2d)
  (setf (aref *opcode-symbols* 137) 'l2f)
  (setf (aref *opcode-symbols* 136) 'l2i)
  (setf (aref *opcode-symbols* 97) 'ladd)
  (setf (aref *opcode-symbols* 47) 'laload)
  (setf (aref *opcode-symbols* 127) 'land)
  (setf (aref *opcode-symbols* 80) 'lastore)
  (setf (aref *opcode-symbols* 148) 'lcmp)
  (setf (aref *opcode-symbols* 9) 'lconst_0)
  (setf (aref *opcode-symbols* 10) 'lconst_1)
  (setf (aref *opcode-symbols* 18) 'ldc)
  (setf (aref *opcode-symbols* 19) 'ldc_w)
  (setf (aref *opcode-symbols* 20) 'ldc2_w)
  (setf (aref *opcode-symbols* 109) 'ldiv)
  (setf (aref *opcode-symbols* 22) 'lload)
  (setf (aref *opcode-symbols* 30) 'lload_0)
  (setf (aref *opcode-symbols* 31) 'lload_1)
  (setf (aref *opcode-symbols* 32) 'lload_2)
  (setf (aref *opcode-symbols* 33) 'lload_3)
  (setf (aref *opcode-symbols* 105) 'lmul)
  (setf (aref *opcode-symbols* 117) 'lneg)
  (setf (aref *opcode-symbols* 171) 'lookupswitch)
  (setf (aref *opcode-symbols* 129) 'lor)
  (setf (aref *opcode-symbols* 113) 'lrem)
  (setf (aref *opcode-symbols* 173) 'lreturn)
  (setf (aref *opcode-symbols* 121) 'lshl)
  (setf (aref *opcode-symbols* 123) 'lshr)
  (setf (aref *opcode-symbols* 55) 'lstore)
  (setf (aref *opcode-symbols* 63) 'lstore_0)
  (setf (aref *opcode-symbols* 64) 'lstore_1)
  (setf (aref *opcode-symbols* 65) 'lstore_2)
  (setf (aref *opcode-symbols* 66) 'lstore_3)
  (setf (aref *opcode-symbols* 101) 'lsub)
  (setf (aref *opcode-symbols* 125) 'lushr)
  (setf (aref *opcode-symbols* 131) 'lxor)
  (setf (aref *opcode-symbols* 194) 'monitorenter)
  (setf (aref *opcode-symbols* 195) 'monitorexit)
  (setf (aref *opcode-symbols* 197) 'multianewarray)
  (setf (aref *opcode-symbols* 187) 'new)
  (setf (aref *opcode-symbols* 188) 'newarray)
  (setf (aref *opcode-symbols* 0) 'nop)
  (setf (aref *opcode-symbols* 87) 'pop)
  (setf (aref *opcode-symbols* 88) 'pop2)
  (setf (aref *opcode-symbols* 181) 'putfield)
  (setf (aref *opcode-symbols* 179) 'putstatic)
  (setf (aref *opcode-symbols* 169) 'ret)
  (setf (aref *opcode-symbols* 177) 'return)
  (setf (aref *opcode-symbols* 53) 'saload)
  (setf (aref *opcode-symbols* 86) 'sastore)
  (setf (aref *opcode-symbols* 17) 'sipush)
  (setf (aref *opcode-symbols* 95) 'swap)
  (setf (aref *opcode-symbols* 170) 'tableswitch)
  (setf (aref *opcode-symbols* 196) 'wide))

(init-opcode-symbols-table)

(defmacro index-from-bytes (code offset)
  `(progn
     (logior (ash (aref ,code (1+ ,offset)) 8)
	     (aref ,code (+ 2 ,offset)))))

(defmacro wide-index-from-bytes (code offset)
  `(logior (ash (aref ,code (1+ ,offset)) 24)
	   (ash (aref ,code (+ 2 ,offset)) 16)
	   (ash (aref ,code (+ 3 ,offset)) 8)
	   (aref ,code (+ 4 ,offset))))

(defun byte-index-from-bytes (code offset)
  (let ((a (aref code (1+ offset)))
        (b (aref code (+ 2 offset))))
    (let ((x (mod (logior (ash a 8) b) 256))) 
      (+ offset (if (> x 127) 
                    (- x 256) 
                    x)))))

(defun skip-padding (offset)
  ;; Skip padding to an address that is a multiple of 4.
  ;; Padding must be between 0 and 3.
  (loop (when (= 0 (mod offset 4)) 
          (return)) 
     (setf offset (1+ offset)))
  offset)

(defun get-lookupswitch (code offset)
  (let ((default-byte (wide-index-from-bytes code offset))
	(match-pairs (make-array (wide-index-from-bytes code (+ 4 offset)))))
    (setf offset (+ 8 offset))
    (dotimes (i (length match-pairs))
      (setf (aref match-pairs i) (cons (wide-index-from-bytes code offset)
                                       (wide-index-from-bytes code (+ 4 offset))))
      (setf offset (+ 8 offset)))
    (cons offset (cons default-byte match-pairs))))

(defstruct table-switch
  (default 0 :type integer)
  (low 0 :type integer)
  (hi 0 :type integer)
  (jump-offsets nil :type simple-array))

(defun get-tableswitch (code offset)
  (let* ((default (wide-index-from-bytes code offset))
	 (low (wide-index-from-bytes code (+ 4 offset)))
	 (hi (wide-index-from-bytes code (+ 8 offset)))
	 (count (1+ (- hi low)))
	 (jump-indices (make-array count)))
    (setf offset (+ 12 offset))
    (dotimes (i count)
      (setf (aref jump-indices i) (aref code offset))
      (setf offset (1+ offset)))
    (cons offset (make-table-switch :default default
				    :low low
				    :hi hi
				    :jump-offsets jump-indices))))

(defun get-wide (code offset)
  (let ((opc (aref *opcode-symbols* (aref code offset))))
    (case opc
      ((iinc)
       (cons (+ 5 offset) (cons (index-from-bytes code (1+ offset))
				(index-from-bytes code (+ 3 offset)))))
      (t (cons (+ 3 offset) (index-from-bytes code (1+ offset)))))))

(defun next-opcode (code offset)
  (let ((opc (aref *opcode-symbols* (aref code offset))))
    (case opc
      ((invokespecial anewarray checkcast getfield getstatic
		      instanceof invokestatic invokevirtual
		      ldc_w ldc2_w new putfield putstatic sipush)
       (cons (+ offset 2) (cons opc (index-from-bytes code offset))))
      ((goto if_acmpne if_acmpeq if_icmpeq if_icmpne if_icmplt 
	     if_icmpge if_icmpgt if_icmple ifeq ifne iflt ifge ifgt ifle
	     ifnonnull ifnull jsr)
       (cons (+ offset 2) (cons opc (byte-index-from-bytes code offset))))
      ((aload astore bipush dload dstore fload fstore iload istore ldc
	      lload lstore newarray ret)
       (cons (1+ offset) (cons opc (aref code (1+ offset)))))
      ((iinc)
       (cons (+ offset 2) (cons opc (cons (aref code (1+ offset)) (aref code (+ 2 offset))))))
      ((invokedynamic)
       (cons (+ offset 4) (cons opc (index-from-bytes code offset))))
      ((invokeinterface)
       (cons (+ offset 4) (cons opc (cons (index-from-bytes code offset)
					  (aref code (+ 3 offset))))))
      ((jsr_w goto_w)
       (cons (+ offset 4) (cons opc (wide-index-from-bytes code offset))))
      ((lookupswitch)
       (get-lookupswitch code (skip-padding offset)))
      ((multianewarray)
       (cons (+ offset 3) (cons opc (cons (index-from-bytes code offset)
					  (aref code (+ 2 offset))))))
      ((tableswitch)
       (get-tableswitch code (skip-padding offset)))
      ((wide)
       (get-wide code offset))
      (t (cons offset opc)))))

(defmacro opcode-offset (self)
  `(car ,self))

(defun opcode-symbol (self)
  (let ((opc (cdr self)))
    (if (consp opc)
        (car opc)
        opc)))

(defun opcode-operands (self)
  (let ((opc (cdr self)))
    (if (consp opc)
        (cdr opc)
        nil)))

(defun bytes-to-opcode (bytes-array)
  (let ((res nil))
    (loop for i from 0 to (1- (length bytes-array))
       do (let ((i-opc (next-opcode bytes-array i)))
	    (setf res (cons (cons i (cdr i-opc)) res))
	    (setf i (car i-opc))))
    (coerce (reverse res) 'simple-vector)))  

(defun opcode-to-string (self)
  (with-output-to-string (s)
    (map nil #'(lambda (opc)
                 (let ((oprnds (opcode-operands opc)))
                   (if (null oprnds)
                       (format s "     ~3a ~15a~%" 
                               (format nil "~a:" (opcode-offset opc)) 
                               (opcode-symbol opc))
                       (format s "     ~3a ~15a  #~a~%" 
                               (format nil "~a:" (opcode-offset opc))
                               (opcode-symbol opc) 
                               oprnds))))
         self)))
