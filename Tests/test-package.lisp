
(defpackage #:eu.turtleware.polyclot/tests
  (:use #:clim-lisp #:alexandria #:eu.turtleware.polyclot #:fiveam)
  (:export #:run-tests
           ;; test suite names
           #:polyclot
           #:polyclot.data-frame))

(in-package #:eu.turtleware.polyclot/tests)

(def-suite polyclot :description "Polyclot tests.")

(defun run-tests (&optional (suite 'polyclot))
  (5am:run! suite))
