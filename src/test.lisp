(in-package :yuka)

(setf s (make-stack 5))
(stack-push s 1)
(stack-push s 2)
(format t "~a~%" (stack-length s))
(stack-push s 3)
(stack-dup2 s)
(format t "~a~%" s)
(loop for i from 0 to (1- (stack-length s))
     do (format t "~a " (stack-at s i)))