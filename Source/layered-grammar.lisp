(in-package #:eu.turtleware.polyclot.impl)

(define-class <aest> ()
  (;; mapping
   (aest :initarg :aest)
   (vars :initarg :vars)
   ;; slots used for caching
   (last-df :initform nil)
   (indexes :initform nil)))

(define-class <stat> ()
  ((aest :initarg :aest)))

;;; <mods> when used in a <stat> context is applied to all rows.
(define-class <mods>     (<stat>) ())
(define-class <identity> (<mods>) ())

(define-class <geom> ()
  ((aest :initarg :aest)))

;;; FIXME stubs for now
(define-class <scale> () ())
(define-class <coord> () ())
(define-class <facet> () ())

(define-class <layer> ()
  ((data :initarg :data)
   (aest :initarg :aest)
   (stat :initarg :stat)
   (geom :initarg :geom)
   (mods :initarg :mods)))

(define-class <chart> (<layer>)
  ((coord :initarg :coord)
   (scale :initarg :scale)
   (facet :initarg :facet)
   (layer :initarg :layer)))

(defun map-aesthetics (aest df row)
  (with-slots (aest vars last-df indexes) aest
    (unless (eql last-df df)
      (setf last-df df
            indexes (map 'list
                         (lambda (col)
                           (position col (cols df) :test #'string=))
                         vars)))
    (loop for aes in aest
          for ind in indexes
          collect aes
          collect (ref df row ind))))

;;; CAUTION modifies a dataframe row
(defun unmap-aesthetics! (aest df row vals)
  (with-slots (aest vars last-df indexes) aest
    (unless (eql last-df df)
      (setf last-df df
            indexes (map 'list
                         (lambda (col)
                           (position col (cols df) :test #'string=))
                         vars)))
    (loop for aes in aest
          for ind in indexes
          for val = (getf vals aes)
          do (setf (elt row ind) val))))

(defgeneric collision-modifier (<stat> vals)
  (:method ((stat <identity>) vals)
    vals))

(defgeneric statistical-transformation (<stat> <data-frame>)
  (:method ((stat <identity>) (frame <data-frame>))
    (declare (ignore stat))
    frame)
  (:method ((stat <mods>) (frame <data-frame>))
    (let ((df (copy-data-frame frame)))
      (flet ((modify-row (index row)
               (let* ((aest (aest stat))
                      (vals (map-aesthetics aest df row))
                      (modified-vals (collision-modifier stat vals)))
                 (unmap-aesthetics! (aest stat) df row modified-vals))))
        (map-data-frame-rows frame t #'modify-row)))))

(defgeneric geometric-object-start  (<geom> <coord> <mods>))
(defgeneric geometric-object-add    (<geom> &rest args))
(defgeneric geometric-object-finish (<geom>))

(defun aest (&rest plist)
  (loop for (i j) on plist by #'cddr
        collect i into aes
        collect j into var
        finally (return (<aest> :aest aes :vars var))))

(defun stat (class &rest args)
  (apply class args))

(defun geom (class &rest args)
  (apply class args))

(defun mods (class &rest args)
  (apply class args))

;;; FIXME macro stub
(defmacro define-chart (name options &rest layers)
  (declare (ignore name options layers)))
