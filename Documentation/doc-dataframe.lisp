(defpackage #:eu.turtleware.polyclot.example
  (:use #:clim-lisp #:alexandria #:eu.turtleware.polyclot))
(in-package #:eu.turtleware.polyclot.example)

;;; This example file follows the data frame tutorial
;;; snippets. SHOW-DATA-FRAME is a helper utility for data frames to
;;; visualise them. If you load this file it will open a few windows,
;;; it is advised to evaluate top level forms step by step when
;;; reading the tutorial.

(defun show-data-frame (data-frame
                        &aux (clim-stream (clim:open-window-stream)))
  (setf (clime:stream-text-margins clim-stream) '(:left 10 :top 5))
  (setf (clim:stream-cursor-position clim-stream)
        (clime:stream-cursor-initial-position clim-stream))
  (clim:formatting-table (clim-stream)
    (clim:formatting-row (clim-stream)
      (clim:surrounding-output-with-border (clim-stream :background clim:+grey+
                                                        :padding-top 0
                                                        :padding-bottom 0)
        (map nil (lambda (column-name)
                   (clim:formatting-cell (clim-stream)
                     (princ column-name clim-stream)))
             (eu.turtleware.polyclot:cols data-frame))))
    (eu.turtleware.polyclot:map-data-frame-rows
     data-frame t
     (lambda (row-index row)
       (declare (ignore row-index))
       (clim:formatting-row (clim-stream)
         (eu.turtleware.polyclot:map-data-frame-cols
          data-frame row t
          (lambda (col-index col-name value)
            (declare (ignore col-index col-name))
            (clim:formatting-cell (clim-stream)
              (print value clim-stream))))))))
  (finish-output clim-stream))

(defparameter *data-frame*
  (make-data-frame '("name"             "address"         "number")
                   ;;---------------------------------------------------
                   '("Chris Richardson" "Franz, Inc."     "510-548-3600")
                   '("Colin Meldrum"    "Franz, Inc."     "510-548-3600")
                   '("Scott McKay"      "Symbolics, Inc." "617-221-1000")
                   '("Bill York"        "Lucid, Inc."     "415-xxx-yyyy")
                   '("Paul Weineke"     "Lucid, Inc."     "415-xxx-yyyy")))

(show-data-frame *data-frame*)

(add-rows! *data-frame*
           '("Daniel Kochmański" "TurtleWare" "516-xxx-yyy")
           '("Anna Durke"        "TurtleWare" "516-xxx-yyy")
           '("Emmy Grahams"      "TurtleWare" "516-xxx-yyy")
           '("Caroline Tarsky"   "Just, Inc." :invalid)
           '("Andrew Smith"      "Just, Inc." :hidden)
           '("David Smith"       "Just, Inc." "666-xxx-yyy")
           '("Emily Bublinski"   "Doit"       "333-xxx-yyy"))

(show-data-frame *data-frame*)

(add-cols! *data-frame*
           "Name length"
           (lambda (row-index row)
             (declare (ignore row-index))
             (length (ref *data-frame* row "name")))
           "Country"
           (lambda (row-index row)
             (declare (ignore row-index))
             (let ((address (ref *data-frame* row "address")))
               (switch (address :test (lambda (elt pat)
                                        (member elt pat :test #'string=)))
                 ('("Franz, Inc." "Symbolics, Inc." "Lucid, Inc.") "USA")
                 ('("TurtleWare" "Just, Inc.")                     "Poland")
                 (otherwise                                        "Unknown")))))

(show-data-frame *data-frame*)

(defparameter *data-frame-copy* (copy-data-frame *data-frame*))
(add-cols! *data-frame-copy* "The answer" (constantly 42))

(show-data-frame *data-frame*)
(show-data-frame *data-frame-copy*)

(defparameter *data-frame-slice*
  (sel *data-frame*
       '(t . 6)
       #("name" "Name length")))

(show-data-frame *data-frame-slice*)

(macrolet ((with-stream (() &body body)
             `(let ((*standard-output* (clim:open-window-stream)))
                (finish-output)
                ,@body
                (finish-output))))
  (with-stream ()
    (map-data-frame
     *data-frame* t #("name")
     (lambda (rind row cind cname value)
       (declare (ignore rind row cind cname))
       (print value))))
  (with-stream ()
    (map-data-frame-rows
     *data-frame* '(2 . t)
     (lambda (rind row)
       (format t "Person id ~s, name ~s~%" rind (ref *data-frame* row "name")))))
  (with-stream ()
    (map-data-frame-cols
     *data-frame* 3 '(1 . t)
     (lambda (cind cname value)
       (format t "~s (~a): ~a~%" cind cname value)))))
