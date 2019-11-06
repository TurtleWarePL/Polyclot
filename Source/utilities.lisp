(in-package #:eu.turtleware.polyclot.impl)

(defmacro define-class (name superclass slots &rest options)
  `(progn
     ,(if-let ((victim (second (find :stealth-mixin options :key #'car))))
        `(stealth-mixin:define-stealth-mixin
          ,name ,superclass ,victim ,slots
          ,@(remove :stealth-mixin options :key #'car))
        `(defclass ,name ,superclass ,slots ,@options))
     (defun ,name (&rest args) (apply #'make-instance ',name args))
     (defvar ,name (find-class ',name))))
