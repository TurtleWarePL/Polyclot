(in-package #:asdf-user)

(defsystem "eu.turtleware.polyclot"
  :description "Library for plotting data with McCLIM inspired by ggplot2."
  :author "Daniel 'jackdaniel' Kochmański"
  :version "0.0.1"
  :license "BSD-2-Clause"
  :depends-on (#:mcclim #:alexandria #:stealth-mixin)
  :in-order-to ((test-op (test-op "eu.turtleware.polyclot/tests")))
  :components ((:module "Documentation"
                :components ((:static-file "documentation.org")
                             (:static-file "doc-dataframe.org")
                             (:static-file "doc-dataframe.lisp")))
               (:module "Source"
                :components ((:file "packages")
                             (:file "utilities")
                             (:file "dataframe")
                             (:file "layered-grammar")
                             (:file "statistical-transformations")))))

(defsystem "eu.turtleware.polyclot/tests"
  :depends-on ("eu.turtleware.polyclot" "fiveam" "alexandria" "mcclim")
  :perform (test-op (operation component)
             (uiop:symbol-call '#:eu.turtleware.polyclot/tests '#:run-tests))
  :pathname "Tests"
  :components ((:file "test-package")
               (:file "test-dataframe")))
