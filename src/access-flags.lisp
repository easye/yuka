(in-package :yuka)

(defconstant +acc-public+ #x0001)
(defconstant +acc-private+ #x0002)
(defconstant +acc-protected+ #x0004)
(defconstant +acc-static+ #x0008)
(defconstant +acc-final+ #x0010)
(defconstant +acc-synchronized+ #x0020)
(defconstant +acc-volatile+ #x0040)
(defconstant +acc-bridge+ #x0040)
(defconstant +acc-transient+ #x0080)
(defconstant +acc-varargs+ #x0080)
(defconstant +acc-native+ #x0100)
(defconstant +acc-abstract+ #x0400)
(defconstant +acc-strict+ #x0800)
(defconstant +acc-synthetic+ #x1000)
(defconstant +acc-enum+ #x4000)

(defmacro flag-set-p (flag i)
  `(not (= 0 (logand ,flag ,i))))

(defun flags-to-string (flag)
  (with-output-to-string (s)
    (when (flag-set-p +acc-public+ flag)
      (format s "public "))
    (when (flag-set-p +acc-private+ flag)
      (format s "private "))
    (when (flag-set-p +acc-protected+ flag)
      (format s "protected "))
    (when (flag-set-p +acc-static+ flag)
      (format s "static "))
    (when (flag-set-p +acc-final+ flag)
      (format s "final "))
    (when (flag-set-p +acc-synchronized+ flag)
      (format s "synchronized "))
    (when (flag-set-p +acc-volatile+ flag)
      (format s "volatile "))
    (when (flag-set-p +acc-bridge+ flag)
      (format s "bridge "))
    (when (flag-set-p +acc-transient+ flag)
      (format s "transient "))
    (when (flag-set-p +acc-varargs+ flag)
      (format s "varargs "))
    (when (flag-set-p +acc-native+ flag)
      (format s "native "))
    (when (flag-set-p +acc-abstract+ flag)
      (format s "abstract "))
    (when (flag-set-p +acc-strict+ flag)
      (format s "strict "))
    (when (flag-set-p +acc-synthetic+ flag)
      (format s "synthetic "))
    (when (flag-set-p +acc-enum+ flag)
      (format s "enum "))))
