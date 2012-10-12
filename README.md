README
======

A Java Virtual Machine written in Common Lisp.
As of now, yuka can load and display a class file in a human-readable format.
Try this from your Common Lisp REPL:

> (load "/home/vijay/Desktop/yuka/src/vm.lisp")
> (yuka:klass-to-string (yuka:load-klass-file "SomeJavaClass.class"))
