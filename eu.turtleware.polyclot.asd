(in-package #:asdf-user)

(defsystem #:eu.turtleware.polyclot
  :depends-on (#:mcclim #:alexandria #:stealth-mixin)
  :components ((:file "packages")
               (:file "utilities")
               (:file "series")
               (:file "charts")
               (:file "panes")
               (:file "format-chart")
               ;; (:file "format-chart")
               (:static-file "documentation.org")
               (:static-file "examples.lisp")))
