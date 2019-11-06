
(defpackage #:eu.turtleware.polyclot
  (:use)
  (:export #:<data-frame> #:<raw-data-frame #:<sel-data-frame>
           #:<invalid-slice> #:<invalid-index> #:<insert-error>
           #:dims #:cols #:rows #:ref #:sel
           #:map-data-frame #:map-data-frame-rows #:map-data-frame-cols
           #:add-cols! #:add-rows!
           #:make-data-frame #:copy-data-frame #:join-data-frame))

(defpackage #:eu.turtleware.polyclot.impl
  (:use #:clim-lisp #:alexandria #:eu.turtleware.polyclot))
